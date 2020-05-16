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
#include "sctracker.h"

void
Decl::Emit()
{
    // Declarations usually don't emit anything.
}

void
VarDecl::Emit()
{
    if (gTypes.find(sym_->tag)->kind() == TypeKind::Struct) {
        EmitPstruct();
        return;
    }

    assert(sym_->ident == iCONSTEXPR);
}

void
VarDecl::EmitPstruct()
{
    if (!init_)
        return;

    auto type = gTypes.find(sym_->tag);
    auto ps = type->asStruct();

    ke::Vector<cell> values;
    values.resize(ps->args.length());

    sym_->codeaddr = code_idx;
    begdseg();

    auto init = init_->AsStructExpr();
    for (const auto& field : init->fields()) {
        auto arg = pstructs_getarg(ps, field.name);
        if (auto expr = field.value->AsStringExpr()) {
            values[arg->index] = (litidx + glb_declared) * sizeof(cell);
            litadd(expr->text()->chars(), expr->text()->length());
        } else if (auto expr = field.value->AsTaggedValueExpr()) {
            values[arg->index] = expr->value();
        } else {
            assert(false);
        }
    }

    sym_->setAddr((litidx + glb_declared) * sizeof(cell));

    for (const auto& value : values)
        litadd(value);

    glb_declared += dumplits();
}

void
Expr::Emit()
{
    AutoErrorPos aep(pos_);

    if (val_.ident == iCONSTEXPR) {
        ldconst(val_.constval, sPRI);
        return;
    }
    DoEmit();
}

void
Expr::EmitTest(bool jump_on_true, int target)
{
    Emit();
    if (jump_on_true)
        jmp_ne0(target);
    else
        jmp_eq0(target);
}

void
IsDefinedExpr::DoEmit()
{
    // Always constant.
    assert(false);
}

void
UnaryExpr::DoEmit()
{
    expr_->Emit();

    // Hack: abort early if the operation was already handled. We really just
    // want to replace the UnaryExpr though.
    if (userop_)
        return;

    switch (token_) {
        case '~':
            invert();
            break;
        case '!':
            lneg();
            break;
        case '-':
            neg();
            break;
        default:
            assert(false);
    }
}

void
PreIncExpr::DoEmit()
{
    expr_->Emit();

    const auto& val = expr_->val();
    value tmp = val;

    if (val.ident != iACCESSOR) {
        if (userop_.sym) {
            emit_userop(userop_, &tmp);
        } else {
            if (token_ == tINC)
                inc(&tmp); /* increase variable first */
            else
                dec(&tmp);
        }
        rvalue(&tmp);  /* and read the result into PRI */
    } else {
        pushreg(sPRI);
        invoke_getter(val.accessor);
        if (userop_.sym) {
            emit_userop(userop_, &tmp);
        } else {
            if (token_ == tINC)
                inc_pri();
            else
                dec_pri();
        }
        popreg(sALT);
        invoke_setter(val.accessor, TRUE);
    }
}

void
PostIncExpr::DoEmit()
{
    expr_->Emit();

    const auto& val = expr_->val();

    if (val.ident == iARRAYCELL || val.ident == iARRAYCHAR || val.ident == iACCESSOR) {
        // Save base address. Stack: [addr]
        pushreg(sPRI);
        // Get pre-inc value.
        rvalue(val);
        // Save pre-inc value, but swap its position with the address.
        popreg(sALT);       // Stack: []
        pushreg(sPRI);      // Stack: [val]
        if (userop_.sym) {
            pushreg(sALT);      // Stack: [val addr]
            // Call the overload.
            pushreg(sPRI);
            markexpr(sPARM, nullptr, 0);
            ffcall(userop_.sym, 1);
            // Restore the address and emit the store.
            popreg(sALT);       // Stack: [val]
            store(&val);
        } else {
            if (val.ident != iACCESSOR)
                moveto1();
            if (token_ == tINC)
                inc(&val);
            else
                dec(&val);
        }
        popreg(sPRI);
    } else {
        // Much simpler case when we don't have to save the base address.
        rvalue(val);
        pushreg(sPRI);
        if (userop_.sym) {
            pushreg(sPRI);
            markexpr(sPARM, nullptr, 0);
            ffcall(userop_.sym, 1);
            store(&val);
        } else {
            if (token_ == tINC)
                inc(&val);
            else
                dec(&val);
        }
        popreg(sPRI);
    }
}

void
BinaryExpr::DoEmit()
{
    assert(!IsChainedOp(token_));

    // We emit constexprs in the |oper_| handler below.
    const auto& left_val = left_->val();
    if (IsAssignOp(token_) || left_val.ident != iCONSTEXPR)
        left_->Emit();

    bool saved_lhs = false;
    if (IsAssignOp(token_)) {
        switch (left_val.ident) {
            case iARRAYCELL:
            case iARRAYCHAR:
            case iARRAY:
            case iREFARRAY:
                if (oper_) {
                    pushreg(sPRI);
                    rvalue(left_val);
                    saved_lhs = true;
                }
                break;
            case iACCESSOR:
                pushreg(sPRI);
                if (oper_)
                    rvalue(left_val);
                saved_lhs = true;
                break;
            default:
                assert(left_->lvalue());
                if (oper_)
                    rvalue(left_val);
                break;
        }

        if (array_copy_length_) {
            assert(!oper_);
            assert(!assignop_.sym);

            pushreg(sPRI);
            right_->Emit();
            popreg(sALT);
            memcopy(array_copy_length_ * sizeof(cell));
            return;
        }
    }

    assert(!array_copy_length_);
    assert(left_val.ident != iARRAY && left_val.ident != iREFARRAY);

    EmitInner(oper_, userop_, left_, right_);

    if (IsAssignOp(token_)) {
        if (saved_lhs)
            popreg(sALT);

        auto tmp = left_val;
        if (assignop_.sym)
            emit_userop(assignop_, nullptr);
        store(&tmp);
    }
}

void
BinaryExpr::EmitInner(OpFunc oper, const UserOperation& in_user_op, Expr* left, Expr* right)
{
    const auto& left_val = left->val();
    const auto& right_val = right->val();

    UserOperation user_op = in_user_op;

    // left goes into ALT, right goes into PRI, though we can swap them for
    // commutative operations.
    if (left_val.ident == iCONSTEXPR) {
        if (right_val.ident == iCONSTEXPR)
            ldconst(right_val.constval, sPRI);
        else
            right->Emit();
        ldconst(left_val.constval, sALT);
    } else {
        // If performing a binary operation, we need to make sure the LHS winds
        // up in ALT. If performing a store, we only need to preserve LHS to
        // ALT if it can't be re-evaluated.
        bool must_save_lhs = oper || !left_val.canRematerialize();
        if (right_val.ident == iCONSTEXPR) {
            if (commutative(oper)) {
                ldconst(right_val.constval, sALT);
                user_op.swapparams ^= true;
            } else {
                if (must_save_lhs)
                    pushreg(sPRI);
                ldconst(right_val.constval, sPRI);
                if (must_save_lhs)
                    popreg(sALT);
            }
        } else {
            if (must_save_lhs)
                pushreg(sPRI);
            right->Emit();
            if (must_save_lhs)
                popreg(sALT);
        }
    }

    if (oper) {
        if (user_op.sym)
            emit_userop(user_op, nullptr);
        else
            oper();
    }
}

// This sets up an outermost context for logical expresions.  There are two
// forms, one for tlAND and one for tlOR.  The bailout label is used for
// short-circuiting.
//
// First, for tlAND such as a && b && ...
//
//    test a
//    jzer bailout
//    test b
//    jzer bailout
//    ...
//    /* all tests are non-zero, so a && b && ... is true */
//    const.pri 1
//    jump done
// bailout:
//    /* some test was zero, so a && b && ... is false */
//    zero.pri
// done:
//
// Then, for tlOR such as a || b || ...
//
//    test a
//    jnz bailout
//    test b
//    jnz bailout
//    ...
//    /* all tests are zero, so a || b || ... is false */
//    zero.pri
//    jump done
// bailout:
//    /* some test was non-zero, so a || b || ... is true */
//    const.pri 1
// done:
//
// When the test is for another LogicalExpression, EmitTest creates an inner
// context.

void
LogicalExpr::DoEmit()
{
    assert(token_ == tlAND || token_ == tlOR);

    ke::Vector<Expr*> sequence;
    FlattenLogical(token_, &sequence);

    int done = getlabel();
    int bailout = getlabel();
    bool jump_on_true = token_ == tlOR;

    for (size_t i = 0; i < sequence.length(); i++)
        sequence[i]->EmitTest(jump_on_true, bailout);

    ldconst(!jump_on_true, sPRI);
    jumplabel(done);
    setlabel(bailout);
    ldconst(jump_on_true, sPRI);
    setlabel(done);
}

// Since we flattened out the tests in DoEmit, any subexpression that's a
// LogicalExpr must not have the same operator as the parent.  Since the 
// operator changed, a short-circuit here shouldn't jump to bailout.
//
// Instead, a short-circuit should fall through to the sibling subexpression.
//
// #1 (a1 && a2 && ...) || (b1 && b2 && ...) || (c1 && c2 && ...)
// #2 (d1 || d2 || ...) && (e1 || e2 || ...) && (f1 || f2 || ...)
//
// In #1, short-circuiting the tlORs should bail out of the entire expression, 
// but short-circuiting the tlANDs means we should move on to the next sibling
// subexpression.  In #2, similarly, short-circuiting the tlANDs bails out,
// but short-circuiting the tlORs should, again, move on to the next sibling.
//
// For #1 --
//
//    test a1
//    jzer fallthrough
//    test a1
//    jzer fallthrough
//    ...
//    /* all tests passed, so our subexpression is true */
//    jump bailout
// fallthrough:
//    /* repeat for bs)
//
// For #2 --
//
//    test d1
//    jnz fallthrough
//    test d2
//    jnz fallthrough
//    ...
//    /* all tests failed, so our subexpression is false */
//    jump bailout
// fallthrough:
//    /* repeat for es */

void
LogicalExpr::EmitTest(bool jump_on_true, int bailout)
{
    assert((jump_on_true && token_ == tlAND) || (jump_on_true && token_ == tlOR));
    assert(token_ == tlAND || token_ == tlOR);

    ke::Vector<Expr*> sequence;
    FlattenLogical(token_, &sequence);

    int fallthrough = getlabel();
    for (size_t i = 0; i < sequence.length(); i++)
        sequence[i]->EmitTest(token_ == tlOR, fallthrough);

    jumplabel(bailout);
    setlabel(fallthrough);
}

void
ChainedCompareExpr::DoEmit()
{
    first_->Emit();

    Expr* left = first_;

    int count = 0;
    for (const auto& op : ops_) {
        // EmitInner() guarantees the right-hand side will be preserved in ALT.
        // emit_userop implicitly guarantees this, as do os_less etc which
        // use XCHG to swap the LHS/RHS expressions.
        if (count)
            relop_prefix();
        BinaryExpr::EmitInner(op.oper, op.userop, left, op.expr);
        if (count)
            relop_suffix();

        left = op.expr;
        count++;
    }
}

void
TernaryExpr::DoEmit()
{
    first_->Emit();

    int flab1 = getlabel();
    int flab2 = getlabel();
    cell_t total1 = 0;
    cell_t total2 = 0;

    pushheaplist();
    jmp_eq0(flab1); /* go to second expression if primary register==0 */

    second_->Emit();

    if ((total1 = pop_static_heaplist())) {
        setheap_save(total1 * sizeof(cell));
    }
    pushheaplist();
    jumplabel(flab2);
    setlabel(flab1);

    third_->Emit();

    if ((total2 = pop_static_heaplist())) {
        setheap_save(total2 * sizeof(cell));
    }
    setlabel(flab2);
    if (val_.ident == iREFARRAY && (total1 && total2)) {
        markheap(MEMUSE_DYNAMIC, 0);
    }
}

void
CastExpr::DoEmit()
{
    expr_->Emit();
}

void
SymbolExpr::DoEmit()
{
    switch (sym_->ident) {
        case iARRAY:
        case iREFARRAY:
            address(sym_, sPRI);
            break;
        case iFUNCTN:
            load_glbfn(sym_);
            sym_->callback = true;
            break;
        case iVARIABLE:
        case iREFERENCE:
            break;
        default:
            // Note: constexprs are handled in Expr::Emit().
            assert(false);
    }
}

void
RvalueExpr::DoEmit()
{
    expr_->Emit();

    value val = expr_->val();
    rvalue(&val);
}

void
CommaExpr::DoEmit()
{
    for (const auto& expr : exprs_)
        expr->Emit();
}

void
ArrayExpr::DoEmit()
{
    auto addr = (litidx + glb_declared) * sizeof(cell);
    for (const auto& expr : exprs_)
        litadd(expr->val().constval);
    ldconst(addr, sPRI);
}

void
ThisExpr::DoEmit()
{
    if (sym_->ident == iREFARRAY)
        address(sym_, sPRI);
}

void
NullExpr::DoEmit()
{
    // Always const.
    assert(false);
}

void
TaggedValueExpr::DoEmit()
{
    // Always const.
    assert(false);
}

void
StringExpr::DoEmit()
{
    auto addr = (litidx + glb_declared) * sizeof(cell);
    litadd(text_->chars(), text_->length());
    ldconst(addr, sPRI);
}

void
IndexExpr::DoEmit()
{
    base_->Emit();

    symbol* sym = base_->val().sym;
    assert(sym);

    bool magic_string = (sym->tag == pc_tag_string && sym->dim.array.level == 0);

    const auto& idxval = expr_->val();
    if (idxval.ident == iCONSTEXPR) {
        if (!(sym->tag == pc_tag_string && sym->dim.array.level == 0)) {
            /* normal array index */
            if (idxval.constval != 0) {
                /* don't add offsets for zero subscripts */
                ldconst(idxval.constval << 2, sALT);
                ob_add();
            }
        } else {
            /* character index */
            if (idxval.constval != 0) {
                /* don't add offsets for zero subscripts */
                ldconst(idxval.constval, sALT); /* 8-bit character */
                ob_add();
            }
        }
    } else {
        pushreg(sPRI);
        expr_->Emit();

        /* array index is not constant */
        if (!magic_string) {
            if (sym->dim.array.length != 0)
                ffbounds(sym->dim.array.length - 1); /* run time check for array bounds */
            else
                ffbounds();
            cell2addr(); /* normal array index */
        } else {
            if (sym->dim.array.length != 0)
                ffbounds(sym->dim.array.length * (32 / sCHARBITS) - 1);
            else
                ffbounds();
            char2addr(); /* character array index */
        }
        popreg(sALT);
        ob_add(); /* base address was popped into secondary register */
    }

    /* the indexed item may be another array (multi-dimensional arrays) */
    if (sym->dim.array.level > 0) {
        /* read the offset to the subarray and add it to the current address */
        value val = base_->val();
        val.ident = iARRAYCELL;
        pushreg(sPRI); /* the optimizer makes this to a MOVE.alt */
        rvalue(&val);
        popreg(sALT);
        ob_add();
    }
}

void
FieldAccessExpr::DoEmit()
{
    assert(token_ == '.');

    // Note that we do not load an iACCESSOR here, we only make sure the base
    // is computed. Emit() never performs loads on l-values, that ability is
    // reserved for RvalueExpr().
    base_->Emit();

    if (field_ && field_->addr()) {
        ldconst(field_->addr() << 2, sALT);
        ob_add();
    }
}

void
SizeofExpr::DoEmit()
{
    // Always a constant.
    assert(false);
}

void
CallExpr::DoEmit()
{
    // If returning an array, push a hidden parameter.
    if (val_.sym) {
        int retsize = array_totalsize(val_.sym);
        assert(retsize > 0  || !cc_ok());

        modheap(retsize * sizeof(cell));
        pushreg(sALT);
        markheap(MEMUSE_STATIC, retsize);
    }

    // Everything heap-allocated after here is owned by the callee.
    pushheaplist();

    for (size_t i = argv_.length() - 1; i < argv_.length(); i--) {
        const auto& expr = argv_[i].expr;
        const auto& arg = argv_[i].arg;

        expr->Emit();

        if (expr->AsDefaultArgExpr()) {
            pushreg(sPRI);
            continue;
        }

        const auto& val = expr->val();
        bool lvalue = expr->lvalue();

        switch (arg->ident) {
            case iVARARGS:
                if (val.ident == iVARIABLE || val.ident == iREFERENCE) {
                    assert(val.sym);
                    assert(lvalue);
                    /* treat a "const" variable passed to a function with a non-const
                     * "variable argument list" as a constant here */
                    if (val.sym->is_const && !arg->is_const) {
                        rvalue(val);
                        setheap_pri();
                    } else if (lvalue) {
                        address(val.sym, sPRI);
                    } else {
                        setheap_pri();
                    }
                } else if (val.ident == iCONSTEXPR || val.ident == iEXPRESSION) {
                    /* allocate a cell on the heap and store the
                     * value (already in PRI) there */
                    setheap_pri();
                }
                if (val.sym)
                    markusage(val.sym, uWRITTEN);
                break;
            case iVARIABLE:
            case iREFARRAY:
                break;
            case iREFERENCE:
                if (val.ident == iVARIABLE || val.ident == iREFERENCE) {
                    assert(val.sym);
                    address(val.sym, sPRI);
                }
                if (val.sym)
                    markusage(val.sym, uWRITTEN);
                break;
            default:
                assert(false);
                break;
        }

        pushreg(sPRI);
        markexpr(sPARM, NULL, 0); // mark the end of a sub-expression
    }

    ffcall(sym_, argv_.length());

    if (val_.sym)
        popreg(sPRI); // Pop hidden parameter as function result

    // Scrap all temporary heap allocations used to perform the call.
    popheaplist(true);
}

void
DefaultArgExpr::DoEmit()
{
    switch (arg_->ident) {
        case iREFARRAY:
        {
            auto& def = arg_->defvalue.array;
            bool is_const = arg_->is_const;

            setdefarray(def.data, def.size, def.arraysize, &def.addr, is_const);
            if (def.data)
                assert(arg_->numdim > 0);
            break;
        }
        case iREFERENCE:
            setheap(arg_->defvalue.val);
            break;
        case iVARIABLE:
            ldconst(arg_->defvalue.val, sPRI);
            break;
        default:
            assert(false);
    }

}

void
CallUserOpExpr::DoEmit()
{
    expr_->Emit();

    if (userop_.oper) {
        auto val = expr_->val();
        emit_userop(userop_, &val);
    } else {
        emit_userop(userop_, nullptr);
    }
}
