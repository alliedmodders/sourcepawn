// vim: set ts=8 sts=4 sw=4 tw=99 et:
//  Pawn compiler - Recursive descend expresion parser
//
//  Copyright (c) ITB CompuPhase, 1997-2005
//
//  This software is provided "as-is", without any express or implied warranty.
//  In no event will the authors be held liable for any damages arising from
//  the use of this software.
//
//  Permission is granted to anyone to use this software for any purpose,
//  including commercial applications, and to alter it and redistribute it
//  freely, subject to the following restrictions:
//
//  1.  The origin of this software must not be misrepresented; you must not
//      claim that you wrote the original software. If you use this software in
//      a product, an acknowledgment in the product documentation would be
//      appreciated but is not required.
//  2.  Altered source versions must be plainly marked as such, and must not be
//      misrepresented as being the original software.
//  3.  This notice may not be removed or altered from any source distribution.
//
//  Version: $Id$

#include "emitter.h"
#include "errors.h"
#include "expressions.h"
#include "lexer.h"
#include "parse-node.h"
#include "sctracker.h"
#include "scvars.h"

void
Stmt::Process()
{
    if (!Bind())
        return;
    if (!Analyze())
        return;
    Emit();
}

bool
StmtList::Analyze()
{
    bool ok = true;
    for (const auto& stmt : stmts_)
        ok &= stmt->Analyze();
    return ok;
}

void
StmtList::Emit()
{
    for (const auto& stmt : stmts_)
        stmt->Emit();
}

bool
Decl::Analyze()
{
    // No analysis needed for most decls.
    return true;
}

bool
VarDecl::Analyze()
{
    if (gTypes.find(sym_->tag)->kind() == TypeKind::Struct)
        return AnalyzePstruct();

    assert(false);
    return false;
}

bool
VarDecl::AnalyzePstruct()
{
    if (!init_)
        return true;

    auto init = init_->AsStructExpr();
    assert(init); // If we parse struct initializers as a normal global, this check will need to be
                  // soft.
    auto type = gTypes.find(sym_->tag);
    auto ps = type->asStruct();

    std::vector<bool> visited;
    visited.resize(ps->args.size());

    // Do as much checking as we can before bailing out.
    bool ok = true;
    for (const auto& field : init->fields())
        ok &= AnalyzePstructArg(ps, field, &visited);

    if (!ok)
        return false;

    // Fill in default values as needed.
    for (size_t i = 0; i < visited.size(); i++) {
        if (visited[i])
            continue;
        if (ps->args[i]->ident == iREFARRAY) {
            assert(ps->args[i]->tag == pc_tag_string);

            auto expr = new StringExpr(pos_, "", 0);
            init->fields().push_back(StructInitField(ps->args[i]->name, expr));
        }
    }

    return true;
}

bool
VarDecl::AnalyzePstructArg(const pstruct_t* ps, const StructInitField& field,
                           std::vector<bool>* visited)
{
    auto arg = pstructs_getarg(ps, field.name);
    if (!arg) {
        error(pos_, 96, field.name->chars(), name_->chars());
        return false;
    }

    if (visited->at(arg->index)) {
        error(field.value->pos(), 58);
        return false;
    }

    visited->at(arg->index) = true;

    if (auto expr = field.value->AsStringExpr()) {
        if (arg->ident != iREFARRAY) {
            error(expr->pos(), 48);
            return false;
        }
        if (arg->tag != pc_tag_string)
            error(expr->pos(), 213, type_to_name(pc_tag_string), type_to_name(arg->tag));
    } else if (auto expr = field.value->AsTaggedValueExpr()) {
        if (arg->ident != iVARIABLE) {
            error(expr->pos(), 23);
            return false;
        }

        // Proper tag checks were missing in the old parser, and unfortunately
        // adding them breaks older code. As a special case, we allow implicit
        // coercion of constants 0 or 1 to bool.
        if (!(arg->tag == pc_tag_bool && expr->tag() == 0 &&
            (expr->value() == 0 || expr->value() == 1)))
        {
            matchtag(arg->tag, expr->tag(), MATCHTAG_COERCE);
        }
    } else {
        assert(false);
        return false;
    }
    return true;
}

bool
ConstDecl::Analyze()
{
    AutoErrorPos aep(pos_);

    matchtag(type_.tag, expr_tag_, 0);
    return true;
}

static inline OpFunc TokenToOpFunc(int token) {
    switch (token) {
        case '*':
        case taMULT:
            return os_mult;
        case '/':
        case taDIV:
            return os_div;
        case '%':
        case taMOD:
            return os_mod;
        case '+':
        case taADD:
            return ob_add;
        case '-':
        case taSUB:
            return ob_sub;
        case tSHL:
        case taSHL:
            return ob_sal;
        case tSHR:
        case taSHR:
            return os_sar;
        case tSHRU:
        case taSHRU:
            return ou_sar;
        case '&':
        case taAND:
            return ob_and;
        case '^':
        case taXOR:
            return ob_xor;
        case '|':
        case taOR:
            return ob_or;
        case tlLE:
            return os_le;
        case tlGE:
            return os_ge;
        case '<':
            return os_lt;
        case '>':
            return os_gt;
        case tlEQ:
            return ob_eq;
        case tlNE:
            return ob_ne;
        case '=':
        case tlOR:
        case tlAND:
            return nullptr;
        default:
            assert(false);
            return nullptr;
    }
}

CompareOp::CompareOp(const token_pos_t& pos, int token, Expr* expr)
  : pos(pos),
    token(token),
    expr(expr),
    oper(TokenToOpFunc(token))
{
}

void
ParseNode::error(const token_pos_t& pos, int number, ...)
{
    va_list ap;
    va_start(ap, number);
    error_va(pos, number, ap);
    va_end(ap);
}

void
Expr::FlattenLogical(int token, std::vector<Expr*>* out)
{
    out->push_back(this);
}


RvalueExpr::RvalueExpr(Expr* expr)
  : EmitOnlyExpr(expr->pos()),
    expr_(expr)
{
    assert(expr_->lvalue());

    val_ = expr_->val();
    if (val_.ident == iACCESSOR) {
        if (val_.accessor->getter)
            markusage(val_.accessor->getter, uREAD);
        val_.ident = iEXPRESSION;
    }
}

void
RvalueExpr::ProcessUses()
{
    expr_->MarkAndProcessUses();
}

bool
IsDefinedExpr::Analyze()
{
    val_.ident = iCONSTEXPR;
    val_.constval = value_;
    val_.tag = 0;
    return true;
}

bool
UnaryExpr::Analyze()
{
    AutoErrorPos aep(pos_);

    if (!expr_->Analyze())
        return false;

    if (expr_->lvalue())
        expr_ = new RvalueExpr(expr_);
    val_ = expr_->val();

    // :TODO: check for invalid types

    UserOperation userop;
    switch (token_) {
        case '~':
            if (val_.ident == iCONSTEXPR)
                val_.constval = ~val_.constval;
            break;
        case '!':
            if (find_userop(lneg, val_.tag, 0, 1, &val_, &userop)) {
                expr_ = new CallUserOpExpr(userop, expr_);
                val_ = expr_->val();
                userop_ = true;
            } else if (val_.ident == iCONSTEXPR) {
                val_.constval = !val_.constval;
            }
            val_.tag = pc_addtag("bool");
            break;
        case '-':
            if (val_.ident == iCONSTEXPR && val_.tag == sc_rationaltag) {
                float f = sp::FloatCellUnion(val_.constval).f32;
                val_.constval = sp::FloatCellUnion(-f).cell;
            } else if (find_userop(neg, val_.tag, 0, 1, &val_, &userop)) {
                expr_ = new CallUserOpExpr(userop, expr_);
                val_ = expr_->val();
                userop_ = true;
            } else if (val_.ident == iCONSTEXPR) {
                /* the negation of a fixed point number is just an integer negation */
                val_.constval = -val_.constval;
            }
            break;
        default:
            assert(false);
    }

    if (val_.ident != iCONSTEXPR)
        val_.ident = iEXPRESSION;
    return true;
}

bool
UnaryExpr::HasSideEffects()
{
    return expr_->HasSideEffects();
}

void
UnaryExpr::ProcessUses()
{
    expr_->MarkAndProcessUses();
}

bool
IncDecExpr::Analyze()
{
    AutoErrorPos aep(pos_);

    if (!expr_->Analyze())
        return false;
    if (!expr_->lvalue()) {
        error(pos_, 22);
        return false;
    }

    const auto& expr_val = expr_->val();
    if (expr_val.ident != iACCESSOR) {
        if (expr_val.sym->is_const) {
            error(pos_, 22); /* assignment to const argument */
            return false;
        }
    } else {
        if (!expr_val.accessor->setter) {
            error(pos_, 152, expr_val.accessor->name);
            return false;
        }
        if (!expr_val.accessor->getter) {
            error(pos_, 149, expr_val.accessor->name);
            return false;
        }
        markusage(expr_val.accessor->getter, uREAD);
        markusage(expr_val.accessor->setter, uREAD);
    }

    auto op = (token_ == tINC) ? user_inc : user_dec;
    find_userop(op, expr_val.tag, 0, 1, &expr_val, &userop_);

    // :TODO: more type checks
    val_.ident = iEXPRESSION;
    val_.tag = expr_val.tag;
    return true;
}

void
IncDecExpr::ProcessUses()
{
    expr_->MarkAndProcessUses();
}

BinaryExprBase::BinaryExprBase(const token_pos_t& pos, int token, Expr* left, Expr* right)
  : Expr(pos),
    token_(token),
    left_(left),
    right_(right)
{
    assert(right_ != this);
}

bool
BinaryExprBase::HasSideEffects()
{
    return left_->HasSideEffects() ||
           right_->HasSideEffects() ||
           IsAssignOp(token_);
}

void
BinaryExprBase::ProcessUses()
{
    left_->MarkAndProcessUses();
    right_->MarkAndProcessUses();
}

BinaryExpr::BinaryExpr(const token_pos_t& pos, int token, Expr* left, Expr* right)
  : BinaryExprBase(pos, token, left, right)
{
    oper_ = TokenToOpFunc(token_);
}

bool
BinaryExpr::HasSideEffects()
{
    if (userop_.sym != nullptr)
        return true;
    return BinaryExprBase::HasSideEffects();
}

bool
BinaryExpr::Analyze()
{
    AutoErrorPos aep(pos_);

    if (!left_->Analyze() || !right_->Analyze())
        return false;

    if (IsAssignOp(token_)) {
        // Mark the left-hand side as written as soon as we can.
        if (symbol* sym = left_->val().sym) {
            markusage(sym, uWRITTEN);
        } else if (auto* accessor = left_->val().accessor) {
            if (!accessor->setter) {
                error(pos_, 152, accessor->name);
                return false;
            }
            markusage(accessor->setter, uREAD);
        }

        if (!ValidateAssignmentLHS())
            return false;
    } else if (left_->lvalue()) {
        left_ = new RvalueExpr(left_);
    }

    // RHS is always loaded. Note we do this after validating the left-hand side,
    // so ValidateAssignment has an original view of RHS.
    if (right_->lvalue())
        right_ = new RvalueExpr(right_);

    const auto& left_val = left_->val();
    const auto& right_val = right_->val();

    if (oper_) {
        assert(token_ != '=');

        if (left_val.ident == iARRAY || left_val.ident == iREFARRAY) {
            const char* ptr = (left_val.sym != NULL) ? left_val.sym->name() : "-unknown-";
            error(pos_, 33, ptr); /* array must be indexed */
            return false;
        }
        if (right_val.ident == iARRAY || right_val.ident == iREFARRAY) {
            const char* ptr = (right_val.sym != NULL) ? right_val.sym->name() : "-unknown-";
            error(pos_, 33, ptr); /* array must be indexed */
            return false;
        }
        /* ??? ^^^ should do same kind of error checking with functions */
    }

    // The assignment operator is overloaded separately.
    if (IsAssignOp(token_)) {
        if (!ValidateAssignmentRHS())
            return false;
    }

    val_.ident = iEXPRESSION;
    val_.tag = left_val.tag;

    if (assignop_.sym)
        val_.tag = assignop_.sym->tag;

    if (oper_) {
        if (find_userop(oper_, left_val.tag, right_val.tag, 2, nullptr, &userop_)) {
            val_.tag = userop_.sym->tag;
        } else if (left_val.ident == iCONSTEXPR && right_val.ident == iCONSTEXPR) {
            char boolresult = FALSE;
            matchtag(left_val.tag, right_val.tag, FALSE);
            val_.ident = iCONSTEXPR;
            val_.constval = calc(left_val.constval, oper_, right_val.constval, &boolresult);
        } else {
            // For the purposes of tag matching, we consider the order to be irrelevant.
            if (!checkval_string(&left_val, &right_val))
                matchtag_commutative(left_val.tag, right_val.tag, MATCHTAG_DEDUCE);
        }

        if (IsChainedOp(token_) || token_ == tlEQ || token_ == tlNE)
            val_.tag = pc_addtag("bool");
    }

    return true;
}

bool
BinaryExpr::ValidateAssignmentLHS()
{
    int left_ident = left_->val().ident;
    if (left_ident == iARRAYCHAR) {
        // This is a special case, assigned to a packed character in a cell
        // is permitted.
        return true;
    }

    if (left_ident == iARRAY || left_ident == iREFARRAY) {
        // array assignment is permitted too (with restrictions)
        if (oper_) {
            error(pos_, 23);
            return false;
        }
        symbol* left_sym = left_->val().sym;
        if (!left_sym) {
            error(pos_, 142);
            return false;
        }
        if (array_totalsize(left_sym) == 0) {
            error(pos_, 46, left_sym->name());
            return false;
        }
        return true;
    }
    if (!left_->lvalue()) {
        error(pos_, 22);
        return false;
    }

    const auto& left_val = left_->val();
    assert(left_val.sym || left_val.accessor);

    // may not change "constant" parameters
    if (left_val.sym && left_val.sym->is_const) {
        error(pos_, 22);
        return false;
    }
    return true;
}

bool
BinaryExpr::ValidateAssignmentRHS()
{
    const auto& left_val = left_->val();
    const auto& right_val = right_->val();

    if (left_val.ident == iVARIABLE) {
        const auto& right_val = right_->val();
        if (right_val.ident == iVARIABLE && right_val.sym == left_val.sym)
            error(pos_, 226, left_val.sym->name()); // self-assignment
    }

    // :TODO: check this comment post-enumstructectomy
    /* Array elements are sometimes considered as sub-arrays --when the
     * array index is an enumeration field and the enumeration size is greater
     * than 1. If the expression on the right side of the assignment is a cell,
     * or if an operation is in effect, this does not apply.
     */
    bool leftarray = left_val.ident == iARRAY ||
                     left_val.ident == iREFARRAY ||
                     ((left_val.ident == iARRAYCELL || left_val.ident == iARRAYCHAR) &&
                       left_val.constval > 1 &&
                       left_val.sym->dim.array.level == 0 &&
                       !oper_ &&
                       (right_val.ident == iARRAY || right_val.ident == iREFARRAY));
    if (leftarray) {
        if (right_val.ident != iARRAY && right_val.ident != iREFARRAY) {
            error(pos_, 47);
            return false;
        }

        bool exact_match = true;
        cell right_length = 0;
        int right_idxtag = 0;
        int left_length = left_val.sym->dim.array.length;
        if (right_val.sym) {
            // Change from the old logic - we immediately reject multi-dimensional
            // arrays in assignment and don't bother validating subarray assignment.
            if (right_val.sym->dim.array.level > 0) {
                error(pos_, 23);
                return false;
            }

            if (right_val.constval == 0)
                right_length = right_val.sym->dim.array.length; // array variable
            else
                right_length = right_val.constval;

            right_idxtag = right_val.sym->x.tags.index;
            if (right_idxtag == 0 && left_val.sym->x.tags.index == 0)
                exact_match = false;
        } else {
            right_length = right_val.constval; // literal array

            // If val is negative, it means that lval2 is a literal string.
            // The string array size may be smaller than the destination
            // array, provided that the destination array does not have an
            // index tag.
            if (right_length < 0) {
                right_length = -right_length;
                if (left_val.sym->x.tags.index == 0)
                    exact_match = false;
            }
        }
        if (left_val.sym->dim.array.level != 0) {
            error(pos_, 47); // array dimensions must match
            return false;
        }
        if (left_length < right_length || (exact_match && left_length > right_length) ||
            right_length == 0)
        {
            error(pos_, 47); // array sizes must match
            return false;
        }
        if (left_val.ident != iARRAYCELL &&
            !matchtag(left_val.sym->x.tags.index, right_idxtag, MATCHTAG_COERCE | MATCHTAG_SILENT))
        {
            error(pos_, 229, right_val.sym ? right_val.sym->name() : left_val.sym->name());
        }
        array_copy_length_ = right_length;
    } else {
        if (right_val.ident == iARRAY || right_val.ident == iREFARRAY) {
            error(pos_, 6); // must be assigned to an array
            return false;
        }

        // Userop tag will be propagated by the caller.
        find_userop(nullptr, left_val.tag, right_val.tag, 2, &left_val, &assignop_);
    }

    if (!oper_ && !checkval_string(&left_val, &right_val)) {
        if ((left_val.tag == pc_tag_string && right_val.tag != pc_tag_string) ||
            (left_val.tag != pc_tag_string && right_val.tag == pc_tag_string))
        {
            error(pos_, 179, type_to_name(left_val.tag), type_to_name(right_val.tag));
            return false;
        }
        matchtag(left_val.tag, right_val.tag, TRUE);
    }
    return true;
}

void
LogicalExpr::FlattenLogical(int token, std::vector<Expr*>* out)
{
    if (token_ == token) {
        left_->FlattenLogical(token, out);
        right_->FlattenLogical(token, out);
    } else {
        Expr::FlattenLogical(token, out);
    }
}

bool
LogicalExpr::Analyze()
{
    AutoErrorPos aep(pos_);

    if (!left_->Analyze() || !right_->Analyze())
        return false;

    if (left_->lvalue())
        left_ = new RvalueExpr(left_);
    if (right_->lvalue())
        right_ = new RvalueExpr(right_);

    const auto& left_val = left_->val();
    const auto& right_val = right_->val();
    if (left_val.ident == iCONSTEXPR && right_val.ident == iCONSTEXPR) {
        val_.ident = iCONSTEXPR;
        if (token_ == tlOR)
            val_.constval = (left_val.constval || right_val.constval);
        else if (token_ == tlAND)
            val_.constval = (left_val.constval && right_val.constval);
        else
            assert(false);
    } else {
        val_.ident = iEXPRESSION;
    }
    val_.sym = nullptr;
    val_.tag = pc_addtag("bool");
    return true;
}

bool
ChainedCompareExpr::HasSideEffects()
{
    if (first_->HasSideEffects())
        return true;
    for (const auto& op : ops_) {
        if (op.userop.sym || op.expr->HasSideEffects())
            return true;
    }
    return false;
}

bool
ChainedCompareExpr::Analyze()
{
    if (!first_->Analyze())
        return false;
    if (first_->lvalue())
        first_ = new RvalueExpr(first_);

    for (auto& op : ops_) {
        if (!op.expr->Analyze())
            return false;
        if (op.expr->lvalue())
            op.expr = new RvalueExpr(op.expr);
    }

    Expr* left = first_;
    bool all_const = (left->val().ident == iCONSTEXPR);
    bool constval = true;

    val_.ident = iEXPRESSION;
    val_.tag = pc_tag_bool;

    for (auto& op : ops_) {
        Expr* right = op.expr;
        const auto& left_val = left->val();
        const auto& right_val = right->val();

        if (left_val.ident == iARRAY || left_val.ident == iREFARRAY) {
            const char* ptr = (left_val.sym != NULL) ? left_val.sym->name() : "-unknown-";
            error(pos_, 33, ptr); /* array must be indexed */
            return false;
        }
        if (right_val.ident == iARRAY || right_val.ident == iREFARRAY) {
            const char* ptr = (right_val.sym != NULL) ? right_val.sym->name() : "-unknown-";
            error(pos_, 33, ptr); /* array must be indexed */
            return false;
        }

        if (find_userop(op.oper, left_val.tag, right_val.tag, 2, nullptr, &op.userop)) {
            if (op.userop.sym->tag != pc_tag_bool) {
                error(op.pos, 51, get_token_string(op.token).c_str());
                return false;
            }
        } else {
            // For the purposes of tag matching, we consider the order to be irrelevant.
            if (!checkval_string(&left_val, &right_val))
                matchtag_commutative(left_val.tag, right_val.tag, MATCHTAG_DEDUCE);
        }

        if (right_val.ident != iCONSTEXPR || op.userop.sym)
            all_const = false;

        // Fold constants as we go.
        if (all_const) {
            switch (op.token) {
                case tlLE:
                    constval &= left_val.constval <= right_val.constval;
                    break;
                case tlGE:
                    constval &= left_val.constval >= right_val.constval;
                    break;
                case '>':
                    constval &= left_val.constval > right_val.constval;
                    break;
                case '<':
                    constval &= left_val.constval < right_val.constval;
                    break;
                default:
                    assert(false);
                    break;
            }
        }

        left = right;
    }

    if (all_const) {
        val_.ident = iCONSTEXPR;
        val_.constval = constval ? 1 :0;
    }
    return true;
}

void
ChainedCompareExpr::ProcessUses()
{
    first_->ProcessUses();
    for (const auto& op : ops_)
        op.expr->ProcessUses();
}

bool
TernaryExpr::Analyze()
{
    AutoErrorPos aep(pos_);

    if (!first_->Analyze() || !second_->Analyze() || !third_->Analyze())
        return false;

    if (first_->lvalue()) {
        first_ = new RvalueExpr(first_);
    } else if (first_->val().ident == iCONSTEXPR) {
        error(pos_, first_->val().constval ? 206 : 205);
    }

    if (second_->lvalue())
        second_ = new RvalueExpr(second_);
    if (third_->lvalue())
        third_ = new RvalueExpr(third_);

    const auto& left = second_->val();
    const auto& right = third_->val();
    bool left_array = (left.ident == iARRAY || right.ident == iREFARRAY);
    bool right_array = (left.ident == iARRAY || right.ident == iREFARRAY);
    if (!left_array && right_array) {
        const char* ptr = "-unknown-";
        if (left.sym != NULL)
            ptr = left.sym->name();
        error(pos_, 33, ptr); /* array must be indexed */
        return false;
    } else if (left_array && !right_array) {
        const char* ptr = "-unknown-";
        if (right.sym != NULL)
            ptr = right.sym->name();
        error(pos_, 33, ptr); /* array must be indexed */
        return false;
    }

    if (!matchtag(left.tag, right.tag, FALSE))
        return false;

    /* If both sides are arrays, we should return the maximal as the lvalue.
     * Otherwise we could buffer overflow and the compiler is too stupid.
     * Literal strings have a constval == -(num_cells) so the cmp is flipped.
     */
    val_ = left;
    if (val_.ident == iARRAY && right.ident == iARRAY && val_.constval < 0 && val_.constval > right.constval)
        val_ = right;

    if (val_.ident == iARRAY)
        val_.ident = iREFARRAY;
    else if (val_.ident != iREFARRAY)
        val_.ident = iEXPRESSION;
    return true;
}

void
TernaryExpr::ProcessUses()
{
    first_->MarkAndProcessUses();
    second_->MarkAndProcessUses();
    third_->MarkAndProcessUses();
}

bool
CastExpr::Analyze()
{
    AutoErrorPos aep(pos_);

    if (tag_ == pc_tag_void) {
        error(pos_, 144);
        return false;
    }

    if (!expr_->Analyze())
        return false;

    val_ = expr_->val();
    lvalue_ = expr_->lvalue();

    Type* ltype = gTypes.find(val_.tag);
    Type* atype = gTypes.find(tag_);
    if (ltype->isObject() || atype->isObject()) {
        matchtag(tag_, val_.tag, MATCHTAG_COERCE);
    } else if (ltype->isFunction() != atype->isFunction()) {
        // Warn: unsupported cast.
        error(pos_, 237);
    } else if (val_.sym && val_.sym->tag == pc_tag_void) {
        error(pos_, 89);
    } else if (atype->isEnumStruct()) {
        error(pos_, 95, atype->name());
    }
    val_.tag = tag_;
    return true;
}

void
CastExpr::ProcessUses()
{
    expr_->MarkAndProcessUses();
}

bool
SymbolExpr::Analyze()
{
    return AnalyzeWithOptions(false);
}

// This is a hack. Most code is not prepared to handle iMETHODMAP in type
// checks, so for now, we forbid it by default. Since the '.' operator *is*
// prepared for this, we have a special analysis option to allow returning
// types as values.
bool
SymbolExpr::AnalyzeWithOptions(bool allow_types)
{
    AutoErrorPos aep(pos_);

    val_.ident = sym_->ident;
    val_.sym = sym_;
    val_.tag = sym_->tag;

    if (sym_->ident == iCONSTEXPR) {
        // Hack: __LINE__ is updated by the lexer, so we have to special case
        // it here.
        static symbol* sLineConst = findconst("__LINE__");
        if (sym_ == sLineConst)
            val_.constval = pos_.line;
        else
            val_.constval = sym_->addr();
    }

    if (sym_->vclass == sGLOBAL && sym_->ident != iFUNCTN) {
        if (!sym_->defined) {
            error(pos_, 17, sym_->name());
            return false;
        }
    }
    if (sym_->ident == iFUNCTN) {
        // If the function is only in the table because it was inserted as
        // a stub in the first pass (used but never declared or implemented),
        // issue an error.
        if (!sym_->prototyped)
            error(pos_, 17, sym_->name());

        if (sym_->native) {
            error(pos_, 76);
            return false;
        }
        if (sym_->array_return()) {
            error(pos_, 182);
            return false;
        }

        funcenum_t* fe = funcenum_for_symbol(sym_);

        // New-style "closure".
        val_.ident = iEXPRESSION;
        val_.tag = fe->tag;
    }

    switch (sym_->ident) {
        case iVARIABLE:
        case iREFERENCE:
            lvalue_ = true;
            break;
        case iARRAY:
        case iREFARRAY:
        case iCONSTEXPR:
        case iFUNCTN:
            // Not an l-value.
            break;
        case iMETHODMAP:
        case iENUMSTRUCT:
            if (!allow_types) {
                error(pos_, 174, sym_->name());
                return false;
            }
            break;
        default:
            // Should not be a symbol.
            assert(false);
    }
    return true;
}

bool
CommaExpr::Analyze()
{
    AutoErrorPos aep(pos_);

    for (const auto& expr : exprs_) {
        if (!expr->Analyze())
            return false;
        has_side_effects_ |= expr->HasSideEffects();
    }

    Expr* last = exprs_.back();
    if (exprs_.size() > 1 && last->lvalue()) {
        last = new RvalueExpr(last);
        exprs_.back() = last;
    }

    val_ = last->val();
    lvalue_ = last->lvalue();

    // Don't propagate a constant if it would cause Emit() to shortcut and not
    // emit other expressions.
    if (exprs_.size() > 1 && val_.ident == iCONSTEXPR)
        val_.ident = iEXPRESSION;
    return true;
}

void
CommaExpr::ProcessUses()
{
    for (const auto& expr : exprs_)
        expr->ProcessUses();
    exprs_.back()->MarkUsed();
}

bool
ArrayExpr::Analyze()
{
    AutoErrorPos aep(pos_);

    int lasttag = -1;
    for (const auto& expr : exprs_) {
        if (!expr->Analyze())
            return false;

        const auto& val = expr->val();
        if (val.ident != iCONSTEXPR) {
            error(pos_, 8);
            return false;
        }
        if (lasttag < 0)
            lasttag = val.tag;
        else
            matchtag(lasttag, val.tag, FALSE);
    }

    val_.ident = iARRAY;
    val_.constval = exprs_.size();
    val_.tag = lasttag;
    return true;
}

bool
IndexExpr::Analyze()
{
    AutoErrorPos aep(pos_);

    if (!base_->Analyze() || !expr_->Analyze())
        return false;
    if (base_->lvalue() && base_->val().ident == iACCESSOR)
        base_ = new RvalueExpr(base_);
    if (expr_->lvalue())
        expr_ = new RvalueExpr(expr_);

    const auto& base = base_->val();
    if (!base.sym) {
        error(pos_, 29);
        return false;
    }
    if (base.sym->ident != iARRAY && base.sym->ident != iREFARRAY) {
        error(pos_, 28, base.sym->name());
        return false;
    }

    if (base.sym->enumroot) {
        if (!matchtag(base.sym->x.tags.index, expr_->val().tag, TRUE))
            return false;
    }

    const auto& expr = expr_->val();
    if (expr.ident == iARRAY || expr.ident == iREFARRAY) {
        error(pos_, 33, expr.sym ? expr.sym->name() : "-unknown-"); /* array must be indexed */
        return false;
    }

    if (gTypes.find(base.sym->x.tags.index)->isEnumStruct()) {
        error(pos_, 117);
        return false;
    }

    int idx_tag = expr_->val().tag;
    if (!is_valid_index_tag(idx_tag)) {
        error(pos_, 77, gTypes.find(idx_tag)->prettyName());
        return false;
    }

    val_ = base_->val();

    if (expr.ident == iCONSTEXPR) {
        if (!(base.sym->tag == pc_tag_string && base.sym->dim.array.level == 0)) {
            /* normal array index */
            if (expr.constval < 0 ||
                (base.sym->dim.array.length != 0 && base.sym->dim.array.length <= expr.constval))
            {
                error(pos_, 32, base.sym->name()); /* array index out of bounds */
                return false;
            }
        } else {
            /* character index */
            if (expr.constval < 0 ||
                (base.sym->dim.array.length != 0 &&
                 base.sym->dim.array.length * ((8 * sizeof(cell)) / sCHARBITS) <=
                     (ucell)expr.constval))
            {
                error(pos_, 32, base.sym->name()); /* array index out of bounds */
                return false;
            }
        }
        /* if the array index is a field from an enumeration, get the tag name
         * from the field and save the size of the field too.
         */
        assert(expr.sym == NULL || expr.sym->dim.array.level == 0);
    }

    if (base.sym->dim.array.level > 0) {
        // Note: Intermediate arrays are not l-values.
        val_.ident = iREFARRAY;
        val_.sym = base.sym->array_child();

        assert(val_.sym != NULL);
        assert(val_.sym->dim.array.level == base.sym->dim.array.level - 1);
        return true;
    }

    /* set type to fetch... INDIRECTLY */
    if (base.sym->tag == pc_tag_string)
        val_.ident = iARRAYCHAR;
    else
        val_.ident = iARRAYCELL;

    val_.tag = base.sym->tag;
    val_.constval = 0;

    lvalue_ = true;
    return true;
}

void
IndexExpr::ProcessUses()
{
    base_->MarkAndProcessUses();
    expr_->MarkAndProcessUses();
}

bool
ThisExpr::Analyze()
{
    assert(sym_->ident == iREFARRAY || sym_->ident == iVARIABLE);

    val_.ident = sym_->ident;
    val_.sym = sym_;
    val_.tag = sym_->tag;
    lvalue_ = (sym_->ident != iREFARRAY);
    return true;
}

bool
NullExpr::Analyze()
{
    val_.ident = iCONSTEXPR;
    val_.constval = 0;
    val_.tag = pc_tag_null_t;
    return true;
}

bool
TaggedValueExpr::Analyze()
{
    val_.ident = iCONSTEXPR;
    val_.tag = tag_;
    val_.constval = value_;
    return true;
}

bool
StringExpr::Analyze()
{
    val_.ident = iARRAY;
    val_.constval = -char_array_cells((cell)text_->length() + 1);
    val_.tag = pc_tag_string;
    return true;
}

bool FieldAccessExpr::Analyze() {
    return AnalyzeWithOptions(false);
}

bool
FieldAccessExpr::AnalyzeWithOptions(bool from_call)
{
    AutoErrorPos aep(pos_);

    if (SymbolExpr* expr = base_->AsSymbolExpr()) {
        if (!expr->AnalyzeWithOptions(true))
            return false;
    } else {
        if (!base_->Analyze())
            return false;
    }

    if (token_ == tDBLCOLON)
        return AnalyzeStaticAccess();

    const auto& base_val = base_->val();
    switch (base_val.ident) {
        case iARRAY:
        case iREFARRAY:
            if (base_val.sym && base_val.sym->dim.array.level == 0) {
                Type* type = gTypes.find(base_val.sym->x.tags.index);
                if (symbol* root = type->asEnumStruct())
                    return AnalyzeEnumStructAccess(type, root, from_call);
            }
            error(pos_, 106);
            return false;
        case iFUNCTN:
            error(pos_, 107);
            return false;
    }

    if (base_val.ident == iMETHODMAP) {
        methodmap_t* map = base_val.sym->methodmap;
        method_ = methodmap_find_method(map, name_->chars());
        if (!method_) {
            error(pos_, 105, map->name, field_->name());
            return false;
        }
        if (!method_->is_static) {
            error(pos_, 176, method_->name, map->name);
            return false;
        }
        val_.ident = iFUNCTN;
        val_.sym = method_->target;
        markusage(method_->target, uREAD);
        return true;
    }

    Type* base_type = gTypes.find(base_val.tag);
    methodmap_t* map = base_type->asMethodmap();
    if (!map) {
        error(pos_, 104, type_to_name(base_val.tag));
        return false;
    }

    method_ = methodmap_find_method(map, name_->chars());
    if (!method_) {
        error(pos_, 105, map->name, name_->chars());
        return false;
    }

    if (method_->getter || method_->setter) {
        // This is the only scenario in which we need to compute a load of the
        // base address. Otherwise, we're only accessing the type.
        if (base_->lvalue())
            base_ = new RvalueExpr(base_);
        val_.ident = iACCESSOR;
        val_.tag = method_->property_tag();
        val_.accessor = method_;
        lvalue_ = true;
        return true;
    }

    if (method_->is_static) {
        error(pos_, 177, method_->name, map->name, method_->name);
        return false;
    }

    val_.ident = iFUNCTN;
    val_.sym = method_->target;
    markusage(method_->target, uREAD);
    return true;
}

void
FieldAccessExpr::ProcessUses()
{
    base_->MarkAndProcessUses();
}

symbol*
FieldAccessExpr::BindCallTarget(int token, Expr** implicit_this)
{
    if (!AnalyzeWithOptions(true))
        return nullptr;
    if (val_.ident != iFUNCTN)
        return nullptr;

    // The static accessor (::) is offsetof(), so it can't return functions.
    assert(token_ == '.');

    if (method_ && method_->parent->ctor == method_) {
        error(pos_, 84, method_->parent->name);
        return nullptr;
    }

    if (base_->lvalue())
        base_ = new RvalueExpr(base_);
    if (field_ || !method_->is_static)
        *implicit_this = base_;
    return val_.sym;
}

symbol*
SymbolExpr::BindCallTarget(int token, Expr** implicit_this)
{
    AutoErrorPos aep(pos_);

    *implicit_this = nullptr;

    if (token != tNEW && sym_->ident == iMETHODMAP && sym_->methodmap) {
        if (!sym_->methodmap->ctor) {
            // Immediately fatal - no function to call.
            error(pos_, 172, sym_->name());
            return nullptr;
        }
        if (sym_->methodmap->must_construct_with_new()) {
            // Keep going, this is basically a style thing.
            error(pos_, 170, sym_->methodmap->name);
            return nullptr;
        }
        return sym_->methodmap->ctor->target;
    }
    if (sym_->ident != iFUNCTN)
        return nullptr;
    return sym_;
}

symbol*
SymbolExpr::BindNewTarget()
{
    AutoErrorPos aep(pos_);

    if (sym_->ident != iMETHODMAP) {
        error(pos_, 116, sym_->name());
        return nullptr;
    }

    methodmap_t* methodmap = sym_->methodmap;
    if (!methodmap->must_construct_with_new()) {
        error(pos_, 171, methodmap->name);
        return nullptr;
    }
    if (!methodmap->ctor) {
        error(pos_, 172, methodmap->name);
        return nullptr;
    }
    return methodmap->ctor->target;
}

bool
FieldAccessExpr::AnalyzeEnumStructAccess(Type* type, symbol* root, bool from_call)
{
    // Enum structs are always arrays, so they're never l-values.
    assert(!base_->lvalue());

    field_ = find_enumstruct_field(type, name_->chars());
    if (!field_) {
        error(pos_, 105, type->name(), name_->chars());
        return false;
    }
    if (field_->ident == iFUNCTN) {
        if (!from_call) {
            error(pos_, 76);
            return false;
        }

        val_.ident = iFUNCTN;
        val_.sym = field_;
        markusage(val_.sym, uREAD);
        return true;
    }
    assert(field_->parent() == root);

    int tag = field_->x.tags.index;

    symbol* var = base_->val().sym;
    if (!var->data())
        var->set_data(std::make_unique<EnumStructVarData>());

    EnumStructVarData* es_var = var->data()->asEnumStructVar();
    es_var->children.push_back(std::make_unique<symbol>(*field_));

    symbol* child = es_var->children.back().get();
    child->setName(name_);
    child->vclass = var->vclass;

    if (gTypes.find(tag)->isEnumStruct()) {
        val_.tag = 0;
        child->tag = 0;
        child->x.tags.index = tag;
    } else {
        val_.tag = tag;
        child->tag = tag;
        child->x.tags.index = 0;
    }

    if (field_->dim.array.length > 0) {
        child->dim.array.length = field_->dim.array.length;
        child->dim.array.slength = field_->dim.array.slength;
        child->dim.array.level = 0;
        child->ident = iREFARRAY;
        val_.constval = field_->dim.array.length;
    } else {
        child->ident = (tag == pc_tag_string) ? iARRAYCHAR : iARRAYCELL;
        val_.constval = 0;
        lvalue_ = true;
    }
    val_.ident = child->ident;
    val_.sym = child;
    return true;
}

bool
FieldAccessExpr::AnalyzeStaticAccess()
{
    AutoErrorPos aep(pos_);

    const auto& base_val = base_->val();
    if (base_val.ident != iENUMSTRUCT) {
        error(pos_, 108);
        return false;
    }

    Type* type = gTypes.find(base_val.tag);
    symbol* field = find_enumstruct_field(type, name_->chars());
    if (!field) {
        error(pos_, 105, type->name(), field_->name());
        return FALSE;
    }
    assert(field->parent() == type->asEnumStruct());

    val_.ident = iCONSTEXPR;
    val_.sym = nullptr;
    val_.constval = field->addr();
    val_.tag = 0;
    return true;
}

bool
FieldAccessExpr::HasSideEffects()
{
    return base_->HasSideEffects() || val_.ident == iACCESSOR;
}

bool
SizeofExpr::Analyze()
{
    AutoErrorPos aep(pos_);

    symbol* sym = sym_;

    markusage(sym, uREAD);

    if (sym->ident == iCONSTEXPR) {
        error(pos_, 39); // constant symbol has no size
        return false;
    } else if (sym->ident == iFUNCTN) {
        error(pos_, 72); // "function" symbol has no size
        return false;
    } else if (!sym->defined) {
        error(pos_, 17, ident_->chars());
        return false;
    }

    val_.ident = iCONSTEXPR;
    val_.constval = 1;

    if (sym->ident == iARRAY || sym->ident == iREFARRAY || sym->ident == iENUMSTRUCT) {
        symbol* subsym = sym;
        for (int level = 0; level < array_levels_; level++) {
            // Forbid index operations on enum structs.
            if (sym->ident == iENUMSTRUCT || gTypes.find(sym->x.tags.index)->isEnumStruct()) {
                error(pos_, 111, sym->name());
                return false;
            }
            if (subsym)
                subsym = subsym->array_child();
        }

        Type* enum_type = nullptr;
        if (suffix_token_ == tDBLCOLON) {
            if (subsym->ident != iENUMSTRUCT) {
                error(pos_, 112, subsym->name());
                return false;
            }
            enum_type = gTypes.find(subsym->tag);
        } else if (suffix_token_ == '.') {
            enum_type = gTypes.find(subsym->x.tags.index);
            if (!enum_type->asEnumStruct()) {
                error(pos_, 116, sym->name());
                return false;
            }
        }

        if (enum_type) {
            assert(enum_type->asEnumStruct());

            symbol* field = find_enumstruct_field(enum_type, field_->chars());
            if (!field) {
                error(pos_, 105, enum_type->name(), field_->chars());
                return false;
            }
            if (int string_size = field->dim.array.slength) {
                val_.constval = string_size;
                return true;
            }
            if (int array_size = field->dim.array.length) {
                val_.constval = array_size;
                return true;
            }
            return true;
        }

        if (sym->ident == iENUMSTRUCT) {
            val_.constval = sym->addr();
            return true;
        }

        if (array_levels_ > sym->dim.array.level + 1) {
            error(pos_, 28, sym->name()); // invalid subscript
            return false;
        }
        if (array_levels_ != sym->dim.array.level + 1) {
            val_.constval = array_levelsize(sym, array_levels_);
            if (val_.constval == 0) {
                error(pos_, 163, sym->name()); // indeterminate array size in "sizeof"
                return false;
            }
        }
    }
    return true;
}

CallUserOpExpr::CallUserOpExpr(const UserOperation& userop, Expr* expr)
  : EmitOnlyExpr(expr->pos()),
    userop_(userop),
    expr_(expr)
{
    val_.ident = iEXPRESSION;
    val_.tag = userop_.sym->tag;
}

void
CallUserOpExpr::ProcessUses()
{
    expr_->MarkAndProcessUses();
}

DefaultArgExpr::DefaultArgExpr(const token_pos_t& pos, arginfo* arg)
  : EmitOnlyExpr(pos),
    arg_(arg)
{
    // Leave val bogus, it doesn't participate in anything, and we can't
    // accurately construct it.
}

bool
CallExpr::Analyze()
{
    AutoErrorPos aep(pos_);

    // Note: we do not Analyze the call target. We leave this to the
    // implementation of BindCallTarget.
    if (token_ == tNEW)
        sym_ = target_->BindNewTarget();
    else
        sym_ = target_->BindCallTarget(token_, &implicit_this_);
    if (!sym_) {
        error(pos_, 12);
        return false;
    }

    markusage(sym_, uREAD);

    // If we're calling a stock in the 2nd pass, and it was never defined as
    // read, then we're encountering some kind of compiler bug. If we're not
    // supposed to emit this code than the status should be statSKIP - so
    // we're generating code that will jump to the wrong address.
    if (sym_->stock && !(sym_->usage & uREAD) && sc_status == statWRITE) {
        error(pos_, 195, sym_->name());
        return false;
    }

    val_.ident = iEXPRESSION;
    val_.tag = sym_->tag;
    if (sym_->array_return()) {
        val_.ident = iREFARRAY;
        val_.sym = sym_->array_return();
    }

    if (sym_->deprecated) {
        const char* ptr = sym_->documentation.c_str();
        error(pos_, 234, sym_->name(), ptr); /* deprecated (probably a native function) */
    }

    unsigned int nargs = 0;
    unsigned int argidx = 0;
    arginfo* arglist = &sym_->function()->args[0];
    if (implicit_this_) {
        if (!ProcessArg(&arglist[0], implicit_this_, 0))
            return false;
        nargs++;
        argidx++;
    }

    bool namedparams = false;
    for (const auto& param : args_) {
        unsigned int argpos;
        if (param.name) {
            int pos = findnamedarg(arglist, param.name->chars());
            if (pos < 0) {
                error(pos_, 17, param.name->chars());
                break;
            }
            argpos = pos;
            argidx = pos;
        } else {
            if (namedparams) {
                error(pos_, 44); // positional parameters must precede named parameters
                return false;
            }
            argpos = nargs;
        }

        if (argpos >= SP_MAX_CALL_ARGUMENTS) {
            error(pos_, 45); // too many function arguments
            return false;
        }
        if (argpos < argv_.size() && argv_[argpos].expr) {
            error(pos_, 58); // argument already set
            return false;
        }
        // Note: we don't do this in ProcessArg, since we don't want to double-call
        // analyze on implicit_this (Analyze is not idempotent).
        if (param.expr && !param.expr->Analyze())
            return false;

        // Add the argument to |argv_| and perform type checks.
        if (!ProcessArg(&arglist[argidx], param.expr, argpos))
            return false;

        assert(argv_[argpos].expr != nullptr);
        nargs++;

        // Don't iterate past terminators (0 or varargs).
        switch (arglist[argidx].ident) {
            case 0:
            case iVARARGS:
                break;
            default:
                argidx++;
                break;
        }
    }

    if (!curfunc) {
        error(pos_, 10);
        return false;
    }

    // Check for missing or invalid extra arguments, and fill in default
    // arguments.
    for (unsigned int argidx = 0; ; argidx++) {
        auto& arg = arglist[argidx];
        if (arg.ident == 0 || arg.ident == iVARARGS)
            break;
        if (argidx >= argv_.size() || !argv_[argidx].expr) {
            if (!ProcessArg(&arg, nullptr, argidx))
                return false;
        }

        Expr* expr = argv_[argidx].expr;
        if (expr->AsDefaultArgExpr() && arg.ident == iVARIABLE) {
            UserOperation userop;
            if (find_userop(nullptr, arg.defvalue_tag, arg.tag, 2, nullptr, &userop))
                argv_[argidx].expr = new CallUserOpExpr(userop, expr);
        }
    }

    return true;
}

bool
CallExpr::ProcessArg(arginfo* arg, Expr* param, unsigned int pos)
{
    while (pos >= argv_.size())
        argv_.push_back(ComputedArg{});

    unsigned int visual_pos = implicit_this_ ? pos : pos + 1;

    if (!param) {
        if (arg->ident == 0 || arg->ident == iVARARGS) {
            error(pos_, 92); // argument count mismatch
            return false;
        }
        if (!arg->hasdefault) {
            error(pos_, 34, visual_pos); // argument has no default value
            return false;
        }

        // The rest of the code to handle default values is in DoEmit.
        argv_[pos].expr = new DefaultArgExpr(pos_, arg);
        argv_[pos].arg = arg;
        return true;
    }

    bool handling_this = implicit_this_ && (pos == 0);

    if (param->val().ident == iACCESSOR) {
        // We must always compute r-values for accessors.
        if (!param->val().accessor->getter) {
            error(param->pos(), 149, param->val().accessor->name);
            return false;
        }
        param = new RvalueExpr(param);
    }

    const auto* val = &param->val();
    bool lvalue = param->lvalue();
    switch (arg->ident) {
        case 0:
            // On the first pass, we don't have all of the parameter info.
            // However, use information must be marked anyway, otherwise
            // vars declared previously will be omitted in the second pass.
            // See SourceMod bug 4643.
            error(pos_, 92); // argument count mismatch
            break;
        case iVARARGS:
            assert(!handling_this);

            // Always pass by reference.
            if (val->ident == iVARIABLE || val->ident == iREFERENCE) {
                if (val->sym->is_const && !arg->is_const) {
                    // Treat a "const" variable passed to a function with a
                    // non-const "variable argument list" as a constant here.
                    if (!lvalue) {
                        error(pos_, 22); // need lvalue
                        return false;
                    }
                }
            }
            if (!checktag_string(arg->tag, val) && !checktag(arg->tag, val->tag))
                error(pos_, 213, type_to_name(arg->tag), type_to_name(val->tag));
            break;
        case iVARIABLE:
        {
            if (val->ident == iFUNCTN || val->ident == iARRAY || val->ident == iREFARRAY) {
                error(pos_, 35, visual_pos); // argument type mismatch
                return false;
            }

            if (lvalue) {
                param = new RvalueExpr(param);
                val = &param->val();
            }

            // Do not allow user operators to transform |this|.
            UserOperation userop;
            if (!handling_this && find_userop(nullptr, val->tag, arg->tag, 2, nullptr, &userop)) {
                param = new CallUserOpExpr(userop, param);
                val = &param->val();
            }
            if (!checktag_string(arg->tag, val))
                checktag(arg->tag, val->tag);
            break;
        }
        case iREFERENCE:
            assert(!handling_this);

            if (!lvalue || val->ident == iARRAYCHAR) {
                error(pos_, 35, visual_pos); // argument type mismatch
                return false;
            }
            if (val->sym && val->sym->is_const && !arg->is_const) {
                error(pos_, 35, visual_pos); // argument type mismatch
                return false;
            }
            checktag(arg->tag, val->tag);
            break;
        case iREFARRAY:
            if (val->ident != iARRAY && val->ident != iREFARRAY && val->ident != iARRAYCELL &&
                val->ident != iARRAYCHAR)
            {
                error(pos_, 35, visual_pos); // argument type mismatch
                return false;
            }
            if (val->sym && val->sym->is_const && !arg->is_const) {
                error(pos_, 35, visual_pos); // argument type mismatch
                return false;
            }
            // Verify that the dimensions match those in |arg|. A literal array
            // always has a single dimension. An iARRAYCELL parameter is also
            // assumed to have a single dimension.
            if (!val->sym || val->ident == iARRAYCELL || val->ident == iARRAYCHAR) {
                if (arg->numdim != 1) {
                    error(pos_, 48); // array dimensions must match
                    return false;
                }
                if (arg->dim[0] != 0) {
                    assert(arg->dim[0] > 0);
                    if (val->ident == iARRAYCELL) {
                        if (val->constval == 0 || arg->dim[0] != val->constval) {
                            error(pos_, 47); // array sizes must match
                            return false;
                        }
                    } else {
                        assert(val->constval != 0); // literal array must have a size
                        if ((val->constval > 0 && arg->dim[0] != val->constval) ||
                            (val->constval < 0 && arg->dim[0] < -val->constval))
                        {
                            error(pos_, 47); // array sizes must match
                            return false;
                        }
                    }
                }
            } else {
                symbol* sym = val->sym;
                if (sym->dim.array.level + 1 != arg->numdim) {
                    error(pos_, 48); // array dimensions must match
                    return false;
                }
                // The lengths for all dimensions must match, unless the dimension
                // length was defined at zero (which means "undefined").
                short level = 0;
                while (sym->dim.array.level > 0) {
                    assert(level < sDIMEN_MAX);
                    if (arg->dim[level] != 0 && sym->dim.array.length != arg->dim[level]) {
                        error(pos_, 47); // array sizes must match
                        return false;
                    }
                    if (!matchtag(arg->idxtag[level], sym->x.tags.index, MATCHTAG_SILENT))
                        error(pos_, 229, sym->name()); // index tag mismatch
                    sym = sym->array_child();
                    level++;
                }
                // The last dimension is checked too, again, unless it is zero.
                assert(level < sDIMEN_MAX);
                if (arg->dim[level] != 0 && sym->dim.array.length != arg->dim[level]) {
                    error(pos_, 47); // array sizes must match
                    return false;
                }
                if (!matchtag(arg->idxtag[level], sym->x.tags.index, MATCHTAG_SILENT)) {
                    // We allow enumstruct -> any[].
                    if (arg->tag != pc_anytag || !gTypes.find(sym->x.tags.index)->asEnumStruct())
                        error(pos_, 229, sym->name());
                }
            }

            checktag(arg->tag, val->tag);
            if ((arg->tag != pc_tag_string && val->tag == pc_tag_string) ||
                (arg->tag == pc_tag_string && val->tag != pc_tag_string))
            {
                error(pos_, 178, type_to_name(val->tag), type_to_name(arg->tag));
                return false;
            }
            break;
        default:
            assert(false);
            break;
    }

    argv_[pos].expr = param;
    argv_[pos].arg = arg;
    return true;
}

void
CallExpr::ProcessUses()
{
    for (const auto& arg : argv_) {
        if (!arg.expr)
            continue;
        arg.expr->MarkAndProcessUses();
    }
}

void
CallExpr::MarkUsed()
{
    if (sym_->defined) {
        /* function is defined, can now check the return value (but make an
         * exception for directly recursive functions)
         */
        if (sym_ != curfunc && !sym_->retvalue) {
            char symname[2 * sNAMEMAX + 16]; /* allow space for user defined operators */
            funcdisplayname(symname, sym_->name());
            error(pos_, 209, symname); /* function should return a value */
        }
    } else {
        /* function not yet defined, set */
        sym_->retvalue = true;
    }
}
