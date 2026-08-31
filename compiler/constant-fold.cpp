/* vim: set ts=8 sts=4 sw=4 tw=99 et: */
/*  Pawn compiler - Recursive descend expresion parser
 *
 *  Copyright (c) ITB CompuPhase, 1997-2005
 *
 *  This software is provided "as-is", without any express or implied warranty.
 *  In no event will the authors be held liable for any damages arising from
 *  the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute it
 *  freely, subject to the following restrictions:
 *
 *  1.  The origin of this software must not be misrepresented; you must not
 *      claim that you wrote the original software. If you use this software in
 *      a product, an acknowledgment in the product documentation would be
 *      appreciated but is not required.
 *  2.  Altered source versions must be plainly marked as such, and must not be
 *      misrepresented as being the original software.
 *  3.  This notice may not be removed or altered from any source distribution.
 *
 *  Version: $Id$
 */
#include <cmath>
#include <type_traits>
#include "compile-context.h"
#include "errors.h"
#include "lexer.h"
#include "parser.h"
#include "sc.h"
#include "semantics.h"
#include "semantics-inl.h"
#include "symbols.h"
#include "types.h"

namespace sp {
namespace cc {

template <typename T>
inline bool IsInRange(int64_t value) {
    return value >= std::numeric_limits<T>::min() && value <= std::numeric_limits<T>::max();
}

ExprVal ConstVal(Type* type, cell value) {
    ExprVal v;
    v.set_constval(QualType(type), value);
    return v;
}

ExprVal ConstVal(Type* type, bool value) {
    return ConstVal(type, value ? 1 : 0);
}

ExprVal ConstVal(Type* type, int64_t value) {
    ExprVal v;
    v.set_const_int64(QualType(type), value);
    return v;
}

ExprVal ConstVal(Type* type, double value) {
    ExprVal v;
    v.set_const_double(QualType(type), value);
    return v;
}

ExprVal ConstVal(Type* type, float value) {
    ExprVal v;
    v.set_const_float(QualType(type), value);
    return v;
}

template <typename T>
static inline bool CheckedAdd(T a, T b, T* result) {
#if defined(__clang__) || defined(__GNUC__)
    return !__builtin_add_overflow(a, b, result);
#elif defined(_MSC_VER)
    using U = std::make_unsigned_t<T>;
    *result = (T)((U)a + (U)b);

    if ((a < 0) != (b < 0))
        return true;
    return (*result < 0) == (a < 0);
#endif
}

template <typename T>
static inline bool CheckedSub(T a, T b, T* result) {
#if defined(__clang__) || defined(__GNUC__)
    return !__builtin_sub_overflow(a, b, result);
#elif defined(_MSC_VER)
    using U = std::make_unsigned_t<T>;
    *result = (T)((U)a - (U)b);

    if ((a < 0) == (b < 0))
        return true;
    return (*result < 0) == (b < 0);
#endif
}

template <typename T>
static inline bool CheckedMul(T a, T b, T* result) {
#if defined(__clang__) || defined(__GNUC__)
    return !__builtin_mul_overflow(a, b, result);
#elif defined(_MSC_VER)
    if (a == 0 || b == 0) {
        *result = 0;
        return true;
    }
    // INT_MIN * -1 breaks the round-trip check below.
    if ((a == std::numeric_limits<T>::min() && b == T(-1)) ||
        (b == std::numeric_limits<T>::min() && a == T(-1))) {
        return false;
    }
    T product = a * b;
    *result = product;
    return product / b == a;
#endif
}

template <typename T>
std::optional<ExprVal> Calc(CompileContext& cc, const token_pos_t& pos, T left,
                            T right, int oper_tok, Type* type)
{
    static_assert(std::is_same_v<T, int32_t> ||
                  std::is_same_v<T, int64_t> ||
                  std::is_same_v<T, double> ||
                  std::is_same_v<T, float>);

    if (IsCompare(oper_tok)) {
        switch (oper_tok) {
            case tlEQ:
                return ConstVal(type, left == right);
            case tlNE:
                return ConstVal(type, left != right);
            case '<':
                return ConstVal(type, left < right);
            case '>':
                return ConstVal(type, left > right);
            case tlLE:
                return ConstVal(type, left <= right);
            case tlGE:
                return ConstVal(type, left >= right);
            default:
                return std::nullopt;
        }
    }

    if constexpr (std::is_integral_v<T>) {
        switch (oper_tok) {
            case '+':
            case '-':
            case '*':
            {
                T result;
                bool safe;
                if (oper_tok == '+')
                    safe = CheckedAdd(left, right, &result);
                else if (oper_tok == '-')
                    safe = CheckedSub(left, right, &result);
                else
                    safe = CheckedMul(left, right, &result);

                if (!safe) {
                    if constexpr (sizeof(T) == sizeof(int64_t)) {
                        report(pos, 97);
                        return std::nullopt;
                    }

                    // The result overflowed, so promote to int64.
                    return Calc<int64_t>(cc, pos, (int64_t)left, (int64_t)right, oper_tok,
                                         cc.types()->type_int64());
                }
                if constexpr (std::is_same_v<T, int32_t>) {
                    // Cell-sized types (int8/int16/int/intptr). For the
                    // narrower source types, promote to the smallest
                    // cell type that fits the result, leaving narrowing
                    // to the assignment check.
                    if (type->isInt8()) {
                        if (!IsInRange<int8_t>(result))
                            return ConstVal(cc.types()->type_int16(), (int16_t)result);
                        return ConstVal(type, (int8_t)result);
                    }
                    if (type->isInt16()) {
                        if (!IsInRange<int16_t>(result))
                            return ConstVal(cc.types()->type_int(), (int32_t)result);
                        return ConstVal(type, (int16_t)result);
                    }
                }
                return ConstVal(type, result);
            }
            case '/':
                if (right == T(0)) {
                    report(pos, 93);
                    return std::nullopt;
                }
                if (left == std::numeric_limits<T>::min() && right == T(-1)) {
                    report(pos, 97);
                    return std::nullopt;
                }
                return ConstVal(type, left / right);
            case '%':
                if (right == T(0)) {
                    report(pos, 93);
                    return std::nullopt;
                }
                if (left == std::numeric_limits<T>::min() && right == T(-1)) {
                    report(pos, 97);
                    return std::nullopt;
                }
                return ConstVal(type, left % right);
            case tSHL: {
                using U = std::conditional_t<sizeof(T) == 4, uint32_t, uint64_t>;
                return ConstVal(type, (T)((U)left << right));
            }
            case tSHR:
                return ConstVal(type, left >> right);
            case tSHRU: {
                using U = std::conditional_t<sizeof(T) == 4, uint32_t, uint64_t>;
                return ConstVal(type, (T)((U)left >> (U)right));
            }
            case '&':
                return ConstVal(type, left & right);
            case '^':
                return ConstVal(type, left ^ right);
            case '|':
                return ConstVal(type, left | right);
            default:
                return std::nullopt;
        }
    } else {
        switch (oper_tok) {
            case '+':
                return ConstVal(type, left + right);
            case '-':
                return ConstVal(type, left - right);
            case '*':
                return ConstVal(type, left * right);
            case '/':
                return ConstVal(type, left / right);
            case '%':
                return ConstVal(type, fmod(left, right));
            default:
                return std::nullopt;
        }
    }
}

bool Expr::FoldToConstant() {
    switch (kind_) {
        case ExprKind::BinaryExpr:
            return to<BinaryExpr>()->FoldToConstant();
        case ExprKind::TernaryExpr:
            return to<TernaryExpr>()->FoldToConstant();
        case ExprKind::CastExpr:
            return to<CastExpr>()->FoldToConstant();
        case ExprKind::SimpleCastExpr:
            return to<SimpleCastExpr>()->FoldToConstant();
        default:
            return false;
    }
}

bool CastExpr::FoldToConstant() {
    cell val;
    Type* from_type;
    if (!expr_->EvalConst(&val, &from_type))
        return false;
    if (type()->isInt16())
        val = (cell_t)(int16_t)val;
    else if (type()->isInt8())
        val = (cell_t)(int8_t)val;
    val_.set_constval(val);
    val_.ident = iCONSTEXPR;
    val_.set_type(type());
    return true;
}

bool SimpleCastExpr::FoldToConstant() {
    cell val;
    Type* from_type;
    if (!from_->EvalConst(&val, &from_type))
        return false;
    if (to_->isFloat() && from_type->coercesToInt()) {
        float f = (float)val;
        val = sp::FloatCellUnion(f).cell;
    }
    val_.set_constval(val);
    val_.ident = iCONSTEXPR;
    val_.set_type(to_);
    return true;
}

bool BinaryExpr::FoldToConstant() {
    if (IsAssignOp(token_))
        return false;

    const ExprVal& left_val = left_->val();
    const ExprVal& right_val = right_->val();

    if (left_val.ident != iCONSTEXPR || right_val.ident != iCONSTEXPR)
        return false;

    Type* left_type = left_val.type();
    Type* right_type = right_val.type();

    auto& cc = CompileContext::get();
    Type* bool_type = cc.types()->type_bool();
    Type* type = IsCompare(token_) ? bool_type : val_.type();

    std::optional<ExprVal> folded;

    if (left_type->isDouble() && right_type->isDouble())
        folded = Calc(cc, pos_, left_val.const_double(), right_val.const_double(), token_, type);
    else if (left_type->isInt64() && right_type->isInt64())
        folded = Calc(cc, pos_, left_val.const_int64(), right_val.const_int64(), token_, type);
    else if (left_type->isIntPtr() && right_type->isIntPtr())
        folded = Calc(cc, pos_, left_val.const_intptr(), right_val.const_intptr(), token_, type);
    else if (left_type->coercesFromInt() && right_type->coercesFromInt())
        folded = Calc(cc, pos_, left_val.const_i32(), right_val.const_i32(), token_, type);

    if (!folded)
        return false;

    val_ = *folded;
    return true;
}

bool TernaryExpr::FoldToConstant() {
    cell cond, left, right;
    if (!first_->EvalConst(&cond, nullptr) || second_->EvalConst(&left, nullptr) ||
        !third_->EvalConst(&right, nullptr))
    {
        return false;
    }

    val_.set_constval(cond ? left : right);
    return true;
}

} // namespace cc
} // namespace sp
