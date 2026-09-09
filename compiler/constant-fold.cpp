/* vim: set ts=8 sts=4 sw=4 tw=99 et: */
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
// Copyright (c) ITB CompuPhase, 1997-2005
//
#include <cmath>
#include <type_traits>

#include <amtl/am-float.h>
#include "constant-fold.h"
#include "compile-context.h"
#include "ir-node.h"
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

std::optional<ExprVal> TryFoldBinary(BinaryExpr* expr, ir::Value* left, ir::Value* right,
                                     Type* type)
{
    int token = expr->token();
    if (IsAssignOp(token))
        return std::nullopt;

    auto left_const = left->as<ir::Constant>();
    auto right_const = right->as<ir::Constant>();
    if (!left_const || !right_const)
        return std::nullopt;

    const ExprVal& left_val = left_const->val();
    const ExprVal& right_val = right_const->val();

    Type* left_type = left_val.type();
    Type* right_type = right_val.type();

    auto& cc = CompileContext::get();

    if (left_type->isDouble() && right_type->isDouble())
        return Calc(cc, expr->pos(), left_val.const_double(), right_val.const_double(), token, type);
    if (left_type->isInt64() && right_type->isInt64())
        return Calc(cc, expr->pos(), left_val.const_int64(), right_val.const_int64(), token, type);
    if (left_type->isIntPtr() && right_type->isIntPtr())
        return Calc(cc, expr->pos(), left_val.const_intptr(), right_val.const_intptr(), token, type);
    if (left_type->coercesFromInt() && right_type->coercesFromInt())
        return Calc(cc, expr->pos(), left_val.const_i32(), right_val.const_i32(), token, type);
    return std::nullopt;
}

bool EvalConst(ir::Value* node, cell* value, Type** type) {
    auto c = node->as<ir::Constant>();
    if (!c)
        return false;

    if (c->val().type()->isWideType() || c->val().type()->isHeapItem())
        return false;

    if (value)
        *value = c->const_cell();
    if (type)
        *type = c->val().type();
    return true;
}

std::optional<bool> FoldToConstantBool(ir::Value* cond) {
    auto c = cond->as<ir::Constant>();
    if (!c)
        return std::nullopt;
    if (c->is_float()) {
        float f = c->const_float();
        return f != 0.0f && !ke::IsNaN(f);
    }
    if (c->is_double()) {
        double d = c->const_double();
        return d != 0.0 && !ke::IsNaN(d);
    }
    if (c->is_intptr())
        return c->const_intptr() != 0;
    if (c->is_int64())
        return c->const_int64() != 0;
    return c->const_cell() != 0;
}

std::optional<ExprVal> TryFoldCast(ir::Value* from_node, Type* to) {
    auto from = from_node->as<ir::Constant>();
    if (!from)
        return std::nullopt;
    if (from->val().type()->isWideType() || from->val().type()->isHeapItem())
        return std::nullopt;

    cell val = from->const_cell();
    if (to->isInt16())
        val = (cell_t)(int16_t)val;
    else if (to->isInt8())
        val = (cell_t)(int8_t)val;

    ExprVal out = {};
    if (to->isInt64())
        out.set_const_int64(to, val);
    else
        out.set_constval(to, val);
    return out;
}

} // namespace cc
} // namespace sp
