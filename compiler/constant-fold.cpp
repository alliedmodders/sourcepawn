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
#include <assert.h>
#include <stdio.h>
#include <stdlib.h> /* for _MAX_PATH */
#include <string.h>
#include "compile-context.h"
#include "errors.h"
#include "constant-fold.h"
#include "lexer.h"
#include "parser.h"
#include "sc.h"
#include "sctracker.h"
#include "semantics.h"
#include "symbols.h"
#include "types.h"

namespace sp {
namespace cc {



cell
calc(cell left, int oper_tok, cell right, char* boolresult)
{
    switch (oper_tok) {
        case '|':
            return (left | right);
        case '^':
            return (left ^ right);
        case '&':
            return (left & right);
        case tlEQ:
            return (left == right);
        case tlNE:
            return (left != right);
        case tSHR:
            return (left >> (int)right);
        case tSHRU:
            return ((ucell)left >> (ucell)right);
        case tSHL:
            return ((ucell)left << (int)right);
        case '+':
            return (left + right);
        case '-':
            return (left - right);
        case '*':
            return (left * right);
        case '/':
            if (right == 0) {
                report(29);
                return 0;
            }
            return left / right;
        case '<':
            *boolresult = true;
            return left < right;
        case '>':
            *boolresult = true;
            return left > right;
        case tlGE:
            *boolresult = true;
            return left >= right;
        case tlLE:
            *boolresult = true;
            return left <= right;
        case '%':
            if (right == 0) {
                report(29);
                return 0;
            }
            return left % right;
    }
    assert(false);
    report(29); /* invalid expression, assumed 0 (this should never occur) */
    return 0;
}

static inline bool
IsTypeBinaryConstantFoldable(Type* type)
{
    if (type->isEnum() || type->isInt())
        return true;
    return false;
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
    cell left_val, right_val;
    Type* left_type;
    Type* right_type;

    if (!left_->EvalConst(&left_val, &left_type) || !right_->EvalConst(&right_val, &right_type))
        return false;
    if (IsAssignOp(token_))
        return false;

    if (!IsTypeBinaryConstantFoldable(left_type) || !IsTypeBinaryConstantFoldable(right_type))
        return false;

    switch (token_) {
        case '*':
            val_.set_constval(left_val * right_val);
            break;
        case '/':
        case '%':
            if (!right_val) {
                report(pos_, 93);
                return false;
            }
            if (left_val == cell(0x80000000) && right_val == -1) {
                report(pos_, 97);
                return false;
            }
            if (token_ == '/')
                val_.set_constval(left_val / right_val);
            else
                val_.set_constval(left_val % right_val);
            break;
        case '+':
            val_.set_constval(left_val + right_val);
            break;
        case '-':
            val_.set_constval(left_val - right_val);
            break;
        case tSHL:
            val_.set_constval(left_val << right_val);
            break;
        case tSHR:
            val_.set_constval(left_val >> right_val);
            break;
        case tSHRU:
            val_.set_constval(uint32_t(left_val) >> uint32_t(right_val));
            break;
        case '&':
            val_.set_constval(left_val & right_val);
            break;
        case '^':
            val_.set_constval(left_val ^ right_val);
            break;
        case '|':
            val_.set_constval(left_val | right_val);
            break;
        default:
            return false;
    }
    return true;
}

bool
TernaryExpr::FoldToConstant()
{
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
