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

#include "array-helpers.h"
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

    sym_->codeaddr = code_idx;

    if (sym_->ident == iCONSTEXPR)
        return;

    if (sym_->vclass == sLOCAL)
        EmitLocal();
    else
        EmitGlobal();
}

void
VarDecl::EmitGlobal()
{
    sym_->setAddr(gDataQueue.dat_address());

    if (sym_->ident == iVARIABLE) {
        assert(!init_ || init_->right()->val().ident == iCONSTEXPR);
        if (init_)
            gDataQueue.Add(init_->right()->val().constval);
        else
            gDataQueue.Add(0);
    } else if (sym_->ident == iARRAY) {
        ArrayData array;
        BuildArrayInitializer(this, &array, gDataQueue.dat_address());

        gDataQueue.Add(std::move(array.iv));
        gDataQueue.Add(std::move(array.data));
        gDataQueue.AddZeroes(array.zeroes);
    } else {
        assert(false);
    }

    // Data queue is only purged in endfunc(), so make sure each global
    // dumps the data queue.
    if (sym_->vclass == sGLOBAL)
        gDataQueue.Emit();
}

void
VarDecl::EmitLocal()
{
    if (sym_->ident == iVARIABLE) {
        declared++;
        sym_->setAddr(-declared * sizeof(cell));
        markexpr(sLDECL, name()->chars(), sym_->addr());
        markstack(MEMUSE_STATIC, 1);

        if (init_) {
            const auto& val = init_->right()->val();
            if (val.ident == iCONSTEXPR) {
                pushval(val.constval);
            } else {
                init_->right()->Emit();
                pushreg(sPRI);
            }
        } else {
            // Note: we no longer honor "decl" for scalars.
            pushval(0);
        }
    } else if (sym_->ident == iARRAY) {
        ArrayData array;
        BuildArrayInitializer(this, &array, 0);

        cell iv_size = array.iv.size();
        cell data_size = array.data.size() + array.zeroes;
        cell total_size = iv_size + data_size;

        declared += total_size;
        sym_->setAddr(-declared * sizeof(cell));
        markexpr(sLDECL, name()->chars(), sym_->addr());
        modstk(-(total_size * sizeof(cell)));
        markstack(MEMUSE_STATIC, total_size);

        cell fill_value = 0;
        cell fill_size = 0;
        if (!array.zeroes) {
            // Check for a fill value as an optimization. Note that zeroes are
            // handled by INITARRAY so we don't bother with zeroes here.
            cell test_value = array.data[0];
            for (size_t i = 1; i < array.data.size(); i++) {
                if (test_value != array.data[i]) {
                    test_value = 0;
                    break;
                }
            }

            if (test_value) {
                // Note: data_size must be preserved since it includes any fills.
                fill_value = test_value;
                fill_size = data_size;
                array.data.clear();
            }
        }

        cell iv_addr = gDataQueue.dat_address();
        gDataQueue.Add(std::move(array.iv));
        gDataQueue.Add(std::move(array.data));
        if (array.zeroes < 16) {
            // For small numbers of extra zeroes, fold them into the data
            // section.
            gDataQueue.AddZeroes(array.zeroes);
            gDataQueue.Compact();
            array.zeroes = 0;
        }

        if (array.zeroes) {
            assert(fill_value == 0);
            fill_size = array.zeroes;
        }

        cell non_filled = data_size - fill_size;

        // the decl keyword is deprecated, but we preserve its optimization for
        // older plugins so we don't introduce any surprises. Note we zap the
        // fill size *after* computing the non-fill size, since we need to
        // compute the copy size correctly.
        if (!autozero_ && fill_size && fill_value == 0)
            fill_size = 0;

        addr_reg(sym_->addr(), sPRI);
        emit_initarray(sPRI, iv_addr, iv_size, non_filled, fill_size, fill_value);
    } else if (sym_->ident == iREFARRAY) {
        // Note that genarray() pushes the address onto the stack, so we don't
        // need to call modstk() here.
        declared++;
        sym_->setAddr(-declared * sizeof(cell));

        markexpr(sLDECL, name()->chars(), sym_->addr());

        if (NewArrayExpr* ctor = init_rhs()->AsNewArrayExpr()) {
            ctor->Emit();
        } else if (StringExpr* ctor = init_rhs()->AsStringExpr()) {
            auto queue_size = gDataQueue.size();
            auto str_addr = gDataQueue.dat_address();
            gDataQueue.Add(ctor->text()->chars(), ctor->text()->length());

            auto cells = gDataQueue.size() - queue_size;
            assert(cells > 0);

            pushval(cells);
            genarray(1, autozero_);
            ldconst(str_addr, sPRI);
            copyarray(sym_, cells * sizeof(cell));
        } else {
            assert(false);
        }

        markheap(MEMUSE_DYNAMIC, 0);
        markstack(MEMUSE_STATIC, 1);
    } else {
        assert(false);
    }

    if (declared + sDEF_AMXSTACK / 2 > pc_stksize)
        pc_stksize = declared + sDEF_AMXSTACK;
}

void
VarDecl::EmitPstruct()
{
    if (!init_)
        return;

    auto type = gTypes.find(sym_->tag);
    auto ps = type->asStruct();

    std::vector<cell> values;
    values.resize(ps->args.size());

    sym_->codeaddr = code_idx;

    auto init = init_rhs()->AsStructExpr();
    for (const auto& field : init->fields()) {
        auto arg = pstructs_getarg(ps, field.name);
        if (auto expr = field.value->AsStringExpr()) {
            values[arg->index] = gDataQueue.dat_address();
            gDataQueue.Add(expr->text()->chars(), expr->text()->length());
        } else if (auto expr = field.value->AsTaggedValueExpr()) {
            values[arg->index] = expr->value();
        } else {
            assert(false);
        }
    }

    sym_->setAddr(gDataQueue.dat_address());

    for (const auto& value : values)
        gDataQueue.Add(value);
    gDataQueue.Emit();
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

void
LogicalExpr::DoEmit()
{
    bool jump_on_true = token_ == tlOR;

    int shortcircuit = getlabel();
    int done = getlabel();

    EmitTest(jump_on_true, shortcircuit);
    ldconst(!jump_on_true, sPRI);
    jumplabel(done);
    setlabel(shortcircuit);
    ldconst(jump_on_true, sPRI);
    setlabel(done);
}

void
LogicalExpr::EmitTest(bool jump_on_true, int target)
{
    std::vector<Expr*> sequence;
    FlattenLogical(token_, &sequence);

    // a || b || c .... given jumpOnTrue, should be:
    //
    //   resolve a
    //   jtrue TAKEN
    //   resolve b
    //   jtrue TAKEN
    //   resolve c
    //   jtrue TAKEN
    //
    // a || b || c .... given jumpOnFalse, should be:
    //   resolve a
    //   jtrue FALLTHROUGH
    //   resolve b
    //   jtrue FALLTHROUGH
    //   resolve c
    //   jfalse TAKEN
    //  FALLTHROUGH:
    //
    // a && b && c ..... given jumpOnTrue, should be:
    //   resolve a
    //   jfalse FALLTHROUGH
    //   resolve b
    //   jfalse FALLTHROUGH
    //   resolve c
    //   jtrue TAKEN
    //  FALLTHROUGH:
    //
    // a && b && c ..... given jumpOnFalse, should be:
    //   resolve a
    //   jfalse TAKEN
    //   resolve b
    //   jfalse TAKEN
    //   resolve c
    //   jfalse TAKEN
    //
    // This is fairly efficient, and by re-entering test() we can ensure each
    // jfalse/jtrue encodes things like "a > b" with a combined jump+compare
    // instruction.
    //
    // Note: to make this slightly easier to read, we make all this logic
    // explicit below rather than collapsing it into a single test() call.

    int fallthrough = getlabel();
    for (size_t i = 0; i < sequence.size() - 1; i++) {
        Expr* expr = sequence[i];
        if (token_ == tlOR) {
            if (jump_on_true)
                expr->EmitTest(true, target);
            else
                expr->EmitTest(true, fallthrough);
        } else {
            assert(token_ == tlAND);
            if (jump_on_true)
                expr->EmitTest(false, fallthrough);
            else
                expr->EmitTest(false, target);
        }
    }

    Expr* last = sequence.back();
    last->EmitTest(jump_on_true, target);
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

    if ((total1 = pop_static_heaplist()))
        setheap_save(total1 * sizeof(cell));

    pushheaplist();
    jumplabel(flab2);
    setlabel(flab1);

    third_->Emit();

    if ((total2 = pop_static_heaplist()))
        setheap_save(total2 * sizeof(cell));

    setlabel(flab2);

    if (val_.ident == iREFARRAY && (total1 && total2))
        markheap(MEMUSE_DYNAMIC, 0);
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
CommaExpr::EmitTest(bool jump_on_true, int target) {
    for (size_t i = 0; i < exprs_.size() - 1; i++)
        exprs_[i]->Emit();

    exprs_.back()->EmitTest(jump_on_true, target);
}

void
ArrayExpr::DoEmit()
{
    auto addr = gDataQueue.dat_address();
    for (const auto& expr : exprs_)
        gDataQueue.Add(expr->val().constval);
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
    auto addr = gDataQueue.dat_address();
    gDataQueue.Add(text_->chars(), text_->length());
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
        if (!magic_string) {
            /* normal array index */
            if (idxval.constval != 0) {
                /* don't add offsets for zero subscripts */
                addconst(idxval.constval << 2);
            }
        } else {
            /* character index */
            if (idxval.constval != 0) {
                /* don't add offsets for zero subscripts */
                addconst(idxval.constval); /* 8-bit character */
            }
        }
    } else {
        pushreg(sPRI);
        expr_->Emit();

        if (sym->dim.array.length != 0)
            ffbounds(sym->dim.array.length - 1); /* run time check for array bounds */
        else
            ffbounds();

        if (magic_string) {
            char2addr();
            popreg(sALT);
            ob_add();
        } else {
            popreg(sALT);
            idxaddr();
        }
    }

    // The indexed item is another array (multi-dimensional arrays).
    if (sym->dim.array.level > 0)
        load_i();
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
        cell retsize = CalcArraySize(val_.sym);

        modheap(retsize * sizeof(cell));
        pushreg(sALT);
        markheap(MEMUSE_STATIC, retsize);
    }

    // Everything heap-allocated after here is owned by the callee.
    pushheaplist();

    for (size_t i = argv_.size() - 1; i < argv_.size(); i--) {
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

    ffcall(sym_, argv_.size());

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
            emit_default_array(arg_);
            break;
        case iREFERENCE:
            setheap(arg_->def->val.get());
            break;
        case iVARIABLE:
            ldconst(arg_->def->val.get(), sPRI);
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

void
NewArrayExpr::DoEmit()
{
    int numdim = 0;
    for (size_t i = 0; i < exprs_.size(); i++) {
        Expr* expr = exprs_[i];
        expr->Emit();

        if (i == exprs_.size() - 1 && tag_ == pc_tag_string)
            stradjust(sPRI);

        pushreg(sPRI);
        numdim++;
    }

    if (symbol* es = gTypes.find(tag_)->asEnumStruct()) {
        // The last dimension is implicit in the size of the enum struct. Note
        // that when synthesizing a NewArrayExpr for old-style declarations,
        // it is impossible to have an enum struct.
        // :TODO: test this
        pushval(es->addr());
        numdim++;
    }

    genarray(numdim, autozero_);
}
