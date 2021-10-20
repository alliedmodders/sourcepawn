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

#include <map>

#include <amtl/am-raii.h>

#include "array-helpers.h"
#include "assembler.h"
#include "code-generator.h"
#include "compile-context.h"
#include "compile-options.h"
#include "errors.h"
#include "expressions.h"
#include "sctracker.h"
#include "symbols.h"

using namespace sp;

#define __ asm_.

CodeGenerator::CodeGenerator(CompileContext& cc, ParseTree* tree)
  : cc_(cc),
    tree_(tree)
{
}

void
CodeGenerator::Generate()
{
    // First instruction is always halt.
    __ emit(OP_HALT, 0);

    deduce_liveness(cc_);

    EmitStmtList(tree_);
}

void
CodeGenerator::AddDebugFile(const std::string& file)
{
    auto str = ke::StringPrintf("F:%x %s", asm_.position(), file.c_str());
    debug_strings_.emplace_back(std::move(str));
}

void
CodeGenerator::AddDebugLine(int linenr)
{
    auto str = ke::StringPrintf("L:%x %x", asm_.position(), linenr);
    if (func_) {
        auto data = func_->function();
        data->dbgstrs.emplace_back(str.c_str(), str.size());
    } else {
        debug_strings_.emplace_back(std::move(str));
    }
}

void
CodeGenerator::AddDebugSymbol(symbol* sym)
{
    auto symname = sym->name();

    /* address tag:name codestart codeend ident vclass [tag:dim ...] */
    assert(sym->ident != iFUNCTN);
    auto string = ke::StringPrintf("S:%x %x:%s %x %x %x %x %x",
                                   sym->addr(), sym->tag, symname, sym->codeaddr,
                                   asm_.position(), sym->ident, sym->vclass, (int)sym->is_const);
    if (sym->ident == iARRAY || sym->ident == iREFARRAY) {
#if !defined NDEBUG
        int count = sym->dim.array.level;
#endif
        symbol* sub;
        string += " [ ";
        for (sub = sym; sub != NULL; sub = sub->array_child()) {
            assert(sub->dim.array.level == count--);
            string += ke::StringPrintf("%x:%x ", sub->x.tags.index, sub->dim.array.length);
        }
        string += "]";
    }

    if (func_) {
        auto data = func_->function();
        data->dbgstrs.emplace_back(string.c_str(), string.size());
    } else {
        debug_strings_.emplace_back(std::move(string));
    }
}

void
CodeGenerator::EmitStmtList(StmtList* list)
{
    for (const auto& stmt : list->stmts())
        EmitStmt(stmt);
}

void
CodeGenerator::EmitStmt(Stmt* stmt)
{
    if (func_) {
        ke::SaveAndSet<int> save_fline(&fline, stmt->pos().line);
        AddDebugLine(stmt->pos().line);
        EmitBreak();
    }

    if (func_)
        pushheaplist(AllocScopeKind::Temp);

    switch (stmt->kind()) {
        case AstKind::ChangeScopeNode:
            EmitChangeScopeNode(stmt->to<ChangeScopeNode>());
            break;
        case AstKind::VarDecl:
            EmitVarDecl(stmt->to<VarDecl>());
            break;
        case AstKind::ExprStmt:
            // Emit even if no side effects.
            EmitExpr(stmt->to<ExprStmt>()->expr());
            break;
        case AstKind::ExitStmt: {
            auto e = stmt->to<ExitStmt>();
            if (e->expr())
                EmitExpr(e->expr());
            else
                __ const_pri(0);
            __ emit(OP_HALT, xEXIT);
            break;
        }
        case AstKind::BlockStmt: {
            auto s = stmt->to<BlockStmt>();
            pushstacklist();
            pushheaplist();

            EmitStmtList(s);

            bool returns = s->flow_type() == Flow_Return;
            popheaplist(!returns);
            popstacklist(!returns);
            break;
        }
        case AstKind::AssertStmt: {
            auto s = stmt->to<AssertStmt>();
            Label flab1;
            EmitTest(s->expr(), true, &flab1);
            __ emit(OP_HALT, xASSERTION);
            __ bind(&flab1);
            break;
        }
        case AstKind::IfStmt:
            EmitIfStmt(stmt->to<IfStmt>());
            break;
        case AstKind::DeleteStmt:
            EmitDeleteStmt(stmt->to<DeleteStmt>());
            break;
        case AstKind::DoWhileStmt:
            EmitDoWhileStmt(stmt->to<DoWhileStmt>());
            break;
        case AstKind::LoopControlStmt:
            EmitLoopControlStmt(stmt->to<LoopControlStmt>());
            break;
        case AstKind::ForStmt:
            EmitForStmt(stmt->to<ForStmt>());
            break;
        case AstKind::SwitchStmt:
            EmitSwitchStmt(stmt->to<SwitchStmt>());
            break;
        case AstKind::FunctionDecl: {
            auto decl = stmt->to<FunctionDecl>();
            EmitFunctionInfo(decl->info());
            break;
        }
        case AstKind::EnumStructDecl:
            EmitEnumStructDecl(stmt->to<EnumStructDecl>());
            break;
        case AstKind::MethodmapDecl:
            EmitMethodmapDecl(stmt->to<MethodmapDecl>());
            break;
        case AstKind::ReturnStmt:
            EmitReturnStmt(stmt->to<ReturnStmt>());
            break;
        case AstKind::TypedefDecl:
        case AstKind::TypesetDecl:
        case AstKind::EnumDecl:
        case AstKind::UsingDecl:
        case AstKind::PstructDecl:
        case AstKind::StaticAssertStmt:
        case AstKind::PragmaUnusedStmt:
            break;
        case AstKind::StmtList:
            EmitStmtList(stmt->to<StmtList>());
            break;

        default:
            assert(false);
    }

    // Scrap all temporary allocations used in the statement, unlesss control
    // flow terminates.
    if (func_)
        popheaplist(stmt->flow_type() == Flow_None);
}

void
CodeGenerator::EmitChangeScopeNode(ChangeScopeNode* node)
{
    AddDebugFile(node->file()->chars());
}

void
CodeGenerator::EmitVarDecl(VarDecl* decl)
{
    symbol* sym = decl->sym();
    if ((sym->is_public || (sym->usage & (uWRITTEN | uREAD)) != 0) && !sym->native)
        AddDebugSymbol(sym);

    if (gTypes.find(sym->tag)->kind() == TypeKind::Struct) {
        EmitPstruct(decl);
        return;
    }

    sym->codeaddr = asm_.position();

    if (sym->ident == iCONSTEXPR)
        return;

    if (sym->vclass == sLOCAL)
        EmitLocalVar(decl);
    else
        EmitGlobalVar(decl);
}

void
CodeGenerator::EmitGlobalVar(VarDecl* decl)
{
    symbol* sym = decl->sym();
    BinaryExpr* init = decl->init();

    sym->setAddr(data_.dat_address());

    if (sym->ident == iVARIABLE) {
        assert(!init || init->right()->val().ident == iCONSTEXPR);
        if (init)
            data_.Add(init->right()->val().constval);
        else
            data_.Add(0);
    } else if (sym->ident == iARRAY) {
        ArrayData array;
        BuildArrayInitializer(decl, &array, data_.dat_address());

        data_.Add(std::move(array.iv));
        data_.Add(std::move(array.data));
        data_.AddZeroes(array.zeroes);
    } else {
        assert(false);
    }
}

void
CodeGenerator::EmitLocalVar(VarDecl* decl)
{
    symbol* sym = decl->sym();
    BinaryExpr* init = decl->init();

    if (sym->ident == iVARIABLE) {
        markstack(MEMUSE_STATIC, 1);
        sym->setAddr(-pc_current_stack * sizeof(cell));

        if (init) {
            const auto& val = init->right()->val();
            if (val.ident == iCONSTEXPR) {
                __ emit(OP_PUSH_C, val.constval);
            } else {
                EmitExpr(init->right());
                __ emit(OP_PUSH_PRI);
            }
        } else {
            // Note: we no longer honor "decl" for scalars.
            __ emit(OP_PUSH_C, 0);
        }
    } else if (sym->ident == iARRAY) {
        ArrayData array;
        BuildArrayInitializer(decl, &array, 0);

        cell iv_size = array.iv.size();
        cell data_size = array.data.size() + array.zeroes;
        cell total_size = iv_size + data_size;

        markstack(MEMUSE_STATIC, total_size);
        sym->setAddr(-pc_current_stack * sizeof(cell));
        __ emit(OP_STACK, -(total_size * sizeof(cell)));

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

        cell iv_addr = data_.dat_address();
        data_.Add(std::move(array.iv));
        data_.Add(std::move(array.data));
        if (array.zeroes < 16) {
            // For small numbers of extra zeroes, fold them into the data
            // section.
            data_.AddZeroes(array.zeroes);
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
        if (!decl->autozero() && fill_size && fill_value == 0)
            fill_size = 0;

        __ emit(OP_ADDR_PRI, sym->addr());
        __ emit(OP_INITARRAY_PRI, iv_addr, iv_size, non_filled, fill_size, fill_value);
    } else if (sym->ident == iREFARRAY) {
        // Note that genarray() pushes the address onto the stack, so we don't
        // need to call modstk() here.
        markheap(MEMUSE_DYNAMIC, 0, AllocScopeKind::Normal);
        markstack(MEMUSE_STATIC, 1);
        sym->setAddr(-pc_current_stack * sizeof(cell));

        auto init_rhs = decl->init_rhs();
        if (NewArrayExpr* ctor = init_rhs->as<NewArrayExpr>()) {
            EmitExpr(ctor);
        } else if (StringExpr* ctor = init_rhs->as<StringExpr>()) {
            auto queue_size = data_.size();
            auto str_addr = data_.dat_address();
            data_.Add(ctor->text()->chars(), ctor->text()->length());

            auto cells = data_.size() - queue_size;
            assert(cells > 0);

            __ emit(OP_PUSH_C, cells);
            if (decl->autozero())
                __ emit(OP_GENARRAY_Z, 1);
            else
                __ emit(OP_GENARRAY, 1);
            __ const_pri(str_addr);
            __ copyarray(sym, cells * sizeof(cell));
        } else {
            assert(false);
        }
    } else {
        assert(false);
    }
}

void
CodeGenerator::EmitPstruct(VarDecl* decl)
{
    if (!decl->init())
        return;

    symbol* sym = decl->sym();
    auto type = gTypes.find(sym->tag);
    auto ps = type->asStruct();

    std::vector<cell> values;
    values.resize(ps->args.size());

    sym->codeaddr = asm_.position();

    auto init = decl->init_rhs()->as<StructExpr>();
    for (const auto& field : init->fields()) {
        auto arg = pstructs_getarg(ps, field.name);
        if (auto expr = field.value->as<StringExpr>()) {
            values[arg->index] = data_.dat_address();
            data_.Add(expr->text()->chars(), expr->text()->length());
        } else if (auto expr = field.value->as<TaggedValueExpr>()) {
            values[arg->index] = expr->value();
        } else if (auto expr = field.value->as<SymbolExpr>()) {
            values[arg->index] = expr->sym()->addr();
        } else {
            assert(false);
        }
    }

    sym->setAddr(data_.dat_address());

    for (const auto& value : values)
        data_.Add(value);
}

void
CodeGenerator::EmitExpr(Expr* expr)
{
    AutoErrorPos aep(expr->pos());

    if (expr->val().ident == iCONSTEXPR) {
        __ const_pri(expr->val().constval);
        return;
    }

    switch (expr->kind()) {
        case AstKind::UnaryExpr:
            EmitUnary(expr->to<UnaryExpr>());
            break;
        case AstKind::IncDecExpr:
            EmitIncDec(expr->to<IncDecExpr>());
            break;
        case AstKind::BinaryExpr:
            EmitBinary(expr->to<BinaryExpr>());
            break;
        case AstKind::LogicalExpr:
            EmitLogicalExpr(expr->to<LogicalExpr>());
            break;
        case AstKind::ChainedCompareExpr:
            EmitChainedCompareExpr(expr->to<ChainedCompareExpr>());
            break;
        case AstKind::TernaryExpr:
            EmitTernaryExpr(expr->to<TernaryExpr>());
            break;
        case AstKind::CastExpr:
            EmitExpr(expr->to<CastExpr>()->expr());
            break;
        case AstKind::SymbolExpr:
            EmitSymbolExpr(expr->to<SymbolExpr>());
            break;
        case AstKind::RvalueExpr: {
            auto e = expr->to<RvalueExpr>();
            EmitExpr(e->expr());
            value val = e->expr()->val();
            EmitRvalue(&val);
            break;
        }
        case AstKind::CommaExpr: {
            auto ce = expr->to<CommaExpr>();
            for (const auto& expr : ce->exprs())
                EmitExpr(expr);
            break;
        }
        case AstKind::ThisExpr: {
            auto e = expr->to<ThisExpr>();
            if (e->sym()->ident == iREFARRAY)
                __ address(e->sym(), sPRI);
            break;
        }
        case AstKind::StringExpr: {
            auto se = expr->to<StringExpr>();
            auto addr = data_.dat_address();
            data_.Add(se->text()->chars(), se->text()->length());
            __ const_pri(addr);
            break;
        }
        case AstKind::ArrayExpr: {
            auto e = expr->to<ArrayExpr>();
            auto addr = data_.dat_address();
            for (const auto& expr : e->exprs())
                data_.Add(expr->val().constval);
            __ const_pri(addr);
            break;
        }
        case AstKind::IndexExpr:
            EmitIndexExpr(expr->to<IndexExpr>());
            break;
        case AstKind::FieldAccessExpr:
            EmitFieldAccessExpr(expr->to<FieldAccessExpr>());
            break;
        case AstKind::CallExpr:
            EmitCallExpr(expr->to<CallExpr>());
            break;
        case AstKind::DefaultArgExpr:
            EmitDefaultArgExpr(expr->to<DefaultArgExpr>());
            break;
        case AstKind::CallUserOpExpr:
            EmitCallUserOpExpr(expr->to<CallUserOpExpr>());
            break;
        case AstKind::NewArrayExpr:
            EmitNewArrayExpr(expr->to<NewArrayExpr>());
            break;

        default:
            assert(false);
    }
}

void
CodeGenerator::EmitTest(Expr* expr, bool jump_on_true, Label* target)
{
    switch (expr->kind()) {
        case AstKind::LogicalExpr:
            EmitLogicalExprTest(expr->to<LogicalExpr>(), jump_on_true, target);
            return;
        case AstKind::UnaryExpr:
            if (EmitUnaryExprTest(expr->to<UnaryExpr>(), jump_on_true, target))
                return;
            break;
        case AstKind::ChainedCompareExpr:
            if (EmitChainedCompareExprTest(expr->to<ChainedCompareExpr>(), jump_on_true, target))
                return;
            break;
        case AstKind::CommaExpr: {
            auto ce = expr->to<CommaExpr>();
            for (size_t i = 0; i < ce->exprs().size() - 1; i++)
                EmitExpr(ce->exprs().at(i));

            EmitTest(ce->exprs().back(), jump_on_true, target);
            return;
        }
    }
    // We need a temporary allocation scope here to cleanup before we branch.
    pushheaplist(AllocScopeKind::Temp);
    EmitExpr(expr);
    popheaplist(true);

    if (jump_on_true)
        __ emit(OP_JNZ, target);
    else
        __ emit(OP_JZER, target);
}

void
CodeGenerator::EmitUnary(UnaryExpr* expr)
{
    EmitExpr(expr->expr());

    // Hack: abort early if the operation was already handled. We really just
    // want to replace the UnaryExpr though.
    if (expr->userop())
        return;

    switch (expr->token()) {
        case '~':
            __ emit(OP_INVERT);
            break;
        case '!':
            __ emit(OP_NOT);
            break;
        case '-':
            __ emit(OP_NEG);
            break;
        default:
            assert(false);
    }
}

bool
CodeGenerator::EmitUnaryExprTest(UnaryExpr* expr, bool jump_on_true, Label* target)
{
    if (!expr->userop() && expr->token() == '!') {
        EmitTest(expr->expr(), !jump_on_true, target);
        return true;
    }
    return false;
}

void
CodeGenerator::EmitIncDec(IncDecExpr* expr)
{
    EmitExpr(expr->expr());

    const auto& val = expr->expr()->val();
    auto& userop = expr->userop();
    value tmp = val;

    if (expr->prefix()) {
        if (val.ident != iACCESSOR) {
            if (userop.sym) {
                EmitUserOp(userop, &tmp);
            } else {
                if (expr->token() == tINC)
                    EmitInc(&tmp); /* increase variable first */
                else
                    EmitDec(&tmp);
            }
            EmitRvalue(&tmp);  /* and read the result into PRI */
        } else {
            __ emit(OP_PUSH_PRI);
            InvokeGetter(val.accessor);
            if (userop.sym) {
                EmitUserOp(userop, &tmp);
            } else {
                if (expr->token() == tINC)
                    __ emit(OP_INC_PRI);
                else
                    __ emit(OP_DEC_PRI);
            }
            __ emit(OP_POP_ALT);
            InvokeSetter(val.accessor, TRUE);
        }
    } else {
        if (val.ident == iARRAYCELL || val.ident == iARRAYCHAR || val.ident == iACCESSOR) {
            // Save base address. Stack: [addr]
            __ emit(OP_PUSH_PRI);
            // Get pre-inc value.
            EmitRvalue(val);
            // Save pre-inc value, but swap its position with the address.
            __ emit(OP_POP_ALT); // Stack: []
            __ emit(OP_PUSH_PRI); // Stack: [val]
            if (userop.sym) {
                __ emit(OP_PUSH_ALT); // Stack: [val addr]
                // Call the overload.
                __ emit(OP_PUSH_PRI);
                EmitCall(userop.sym, 1);
                // Restore the address and emit the store.
                __ emit(OP_POP_ALT);
                EmitStore(&val);
            } else {
                if (val.ident != iACCESSOR)
                    __ emit(OP_MOVE_PRI);
                if (expr->token() == tINC)
                    EmitInc(&val);
                else
                    EmitDec(&val);
            }
            __ emit(OP_POP_PRI);
        } else {
            // Much simpler case when we don't have to save the base address.
            EmitRvalue(val);
            __ emit(OP_PUSH_PRI);
            if (userop.sym) {
                __ emit(OP_PUSH_PRI);
                EmitCall(userop.sym, 1);
                EmitStore(&val);
            } else {
                if (expr->token() == tINC)
                    EmitInc(&val);
                else
                    EmitDec(&val);
            }
            __ emit(OP_POP_PRI);
        }
    }
}

void
CodeGenerator::EmitBinary(BinaryExpr* expr)
{
    auto token = expr->token();
    auto left = expr->left();
    auto right = expr->right();
    auto oper = expr->oper();

    assert(!IsChainedOp(token));

    // We emit constexprs in the |oper_| handler below.
    const auto& left_val = left->val();
    if (IsAssignOp(token) || left_val.ident != iCONSTEXPR)
        EmitExpr(left);

    bool saved_lhs = false;
    if (IsAssignOp(token)) {
        switch (left_val.ident) {
            case iARRAYCELL:
            case iARRAYCHAR:
            case iARRAY:
            case iREFARRAY:
                if (oper) {
                    __ emit(OP_PUSH_PRI);
                    EmitRvalue(left_val);
                    saved_lhs = true;
                }
                break;
            case iACCESSOR:
                __ emit(OP_PUSH_PRI);
                if (oper)
                    EmitRvalue(left_val);
                saved_lhs = true;
                break;
            default:
                assert(left->lvalue());
                if (oper)
                    EmitRvalue(left_val);
                break;
        }

        if (expr->array_copy_length()) {
            assert(!oper);
            assert(!expr->assignop().sym);

            __ emit(OP_PUSH_PRI);
            EmitExpr(right);
            __ emit(OP_POP_ALT);
            __ emit(OP_MOVS, expr->array_copy_length() * sizeof(cell));
            return;
        }
    }

    assert(!expr->array_copy_length());
    assert(left_val.ident != iARRAY && left_val.ident != iREFARRAY);

    EmitBinaryInner(oper, expr->userop(), left, right);

    if (IsAssignOp(token)) {
        if (saved_lhs)
            __ emit(OP_POP_ALT);

        auto tmp = left_val;
        if (expr->assignop().sym)
            EmitUserOp(expr->assignop(), nullptr);
        EmitStore(&tmp);
    }
}

void
CodeGenerator::EmitBinaryInner(int oper_tok, const UserOperation& in_user_op, Expr* left,
                               Expr* right)
{
    const auto& left_val = left->val();
    const auto& right_val = right->val();

    UserOperation user_op = in_user_op;

    // left goes into ALT, right goes into PRI, though we can swap them for
    // commutative operations.
    if (left_val.ident == iCONSTEXPR) {
        if (right_val.ident == iCONSTEXPR)
            __ const_pri(right_val.constval);
        else
            EmitExpr(right);
        __ const_alt(left_val.constval);
    } else {
        // If performing a binary operation, we need to make sure the LHS winds
        // up in ALT. If performing a store, we only need to preserve LHS to
        // ALT if it can't be re-evaluated.
        bool must_save_lhs = oper_tok || !left_val.canRematerialize();
        if (right_val.ident == iCONSTEXPR) {
            if (commutative(oper_tok)) {
                __ const_alt(right_val.constval);
                user_op.swapparams ^= true;
            } else {
                if (must_save_lhs)
                    __ emit(OP_PUSH_PRI);
                __ const_pri(right_val.constval);
                if (must_save_lhs)
                    __ emit(OP_POP_ALT);
            }
        } else {
            if (must_save_lhs)
                __ emit(OP_PUSH_PRI);
            EmitExpr(right);
            if (must_save_lhs)
                __ emit(OP_POP_ALT);
        }
    }

    if (oper_tok) {
        if (user_op.sym) {
            EmitUserOp(user_op, nullptr);
            return;
        }
        switch (oper_tok) {
            case '*':
                __ emit(OP_SMUL);
                break;
            case '/':
                __ emit(OP_SDIV_ALT);
                break;
            case '%':
                __ emit(OP_SDIV_ALT);
                __ emit(OP_MOVE_PRI);
                break;
            case '+':
                __ emit(OP_ADD);
                break;
            case '-':
                __ emit(OP_SUB_ALT);
                break;
            case tSHL:
                __ emit(OP_XCHG);
                __ emit(OP_SHL);
                break;
            case tSHR:
                __ emit(OP_XCHG);
                __ emit(OP_SSHR);
                break;
            case tSHRU:
                __ emit(OP_XCHG);
                __ emit(OP_SHR);
                break;
            case '&':
                __ emit(OP_AND);
                break;
            case '^':
                __ emit(OP_XOR);
                break;
            case '|':
                __ emit(OP_OR);
                break;
            case tlLE:
                __ emit(OP_XCHG);
                __ emit(OP_SLEQ);
                break;
            case tlGE:
                __ emit(OP_XCHG);
                __ emit(OP_SGEQ);
                break;
            case '<':
                __ emit(OP_XCHG);
                __ emit(OP_SLESS);
                break;
            case '>':
                __ emit(OP_XCHG);
                __ emit(OP_SGRTR);
                break;
            case tlEQ:
                __ emit(OP_EQ);
                break;
            case tlNE:
                __ emit(OP_NEQ);
                break;
            default:
                assert(false);
        }
    }
}

void
CodeGenerator::EmitLogicalExpr(LogicalExpr* expr)
{
    bool jump_on_true = expr->token() == tlOR;

    Label shortcircuit, done;

    EmitTest(expr, jump_on_true, &shortcircuit);
    __ const_pri(!jump_on_true);
    __ emit(OP_JUMP, &done);
    __ bind(&shortcircuit);
    __ const_pri(jump_on_true);
    __ bind(&done);
}

void
CodeGenerator::EmitLogicalExprTest(LogicalExpr* root, bool jump_on_true, Label* target)
{
    std::vector<Expr*> sequence;
    root->FlattenLogical(root->token(), &sequence);

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

    Label fallthrough;
    for (size_t i = 0; i < sequence.size() - 1; i++) {
        auto expr = sequence.at(i);
        if (root->token() == tlOR) {
            if (jump_on_true)
                EmitTest(expr, true, target);
            else
                EmitTest(expr, true, &fallthrough);
        } else {
            assert(root->token() == tlAND);
            if (jump_on_true)
                EmitTest(expr, false, &fallthrough);
            else
                EmitTest(expr, false, target);
        }
    }

    Expr* last = sequence.back();
    EmitTest(last, jump_on_true, target);
    __ bind(&fallthrough);
}

static inline OPCODE
CmpTokenToOp(int token)
{
    switch (token) {
        case tlGE:
            return OP_JSGEQ;
        case tlLE:
            return OP_JSLEQ;
        case '<':
            return OP_JSLESS;
        case '>':
            return OP_JSGRTR;
        default:
            assert(false);
            return OP_HALT;
    }
}

bool
CodeGenerator::EmitChainedCompareExprTest(ChainedCompareExpr* root, bool jump_on_true,
                                          Label* target)
{
    // No optimization for user operators or for compare chains.
    if (root->ops().size() > 1 || root->ops()[0].userop.sym)
        return false;

    Expr* left = root->first();
    Expr* right = root->ops()[0].expr;

    pushheaplist(AllocScopeKind::Temp);

    EmitExpr(right);
    __ emit(OP_PUSH_PRI);
    EmitExpr(left);
    __ emit(OP_POP_ALT);

    popheaplist(true);

    int token = root->ops()[0].token;
    if (!jump_on_true) {
        switch (token) {
            case '<':
                token = tlGE;
                break;
            case '>':
                token = tlLE;
                break;
            case tlGE:
                token = '<';
                break;
            case tlLE:
                token = '>';
                break;
            default:
                assert(false);
        }
    }
    __ emit(CmpTokenToOp(token), target);
    return true;
}

void
CodeGenerator::EmitChainedCompareExpr(ChainedCompareExpr* root)
{
    EmitExpr(root->first());

    Expr* left = root->first();

    int count = 0;
    for (const auto& op : root->ops()) {
        // EmitInner() guarantees the right-hand side will be preserved in ALT.
        // EmitUserOp implicitly guarantees this, as do os_less etc which
        // use XCHG to swap the LHS/RHS expressions.
        if (count)
            __ relop_prefix();
        EmitBinaryInner(op.oper_tok, op.userop, left, op.expr);
        if (count)
            __ relop_suffix();

        left = op.expr;
        count++;
    }
}

void
CodeGenerator::EmitTernaryExpr(TernaryExpr* expr)
{
    EmitExpr(expr->first());

    Label flab1, flab2;

    pushheaplist(AllocScopeKind::Temp);
    __ emit(OP_JZER, &flab1);

    EmitExpr(expr->second());

    // We unconditionally pop the heaplist here. This is unfortunate, but we
    // don't have many options given how the static verifier works. If the
    // first branch does not use the heap, but the second one does, the tracker
    // balance will be inequal across both branches.
    //
    // We can't scrap generated code because we might have already bound labels
    // (for example, to the addresses of global functions).
    //
    // So instead, we unconditionally generate TRACKER_PUSH_C instructions.
    //
    // In the future, we should refactor how the verifier works so we can
    // simplify this.
    auto total1 = pop_static_heaplist();
    __ emit(OP_TRACKER_PUSH_C, total1 * sizeof(cell));

    pushheaplist(AllocScopeKind::Temp);
    __ emit(OP_JUMP, &flab2);
    __ bind(&flab1);

    EmitExpr(expr->third());

    auto total2 = pop_static_heaplist();
    __ emit(OP_TRACKER_PUSH_C, total2 * sizeof(cell));
    __ bind(&flab2);

    // Remove this hack when we can add heap information in the analysis phase.
    if (total1 || total2)
        markheap(MEMUSE_DYNAMIC, 0, AllocScopeKind::Temp);
    else
        __ emit(OP_TRACKER_POP_SETHEAP);
}

void
CodeGenerator::EmitSymbolExpr(SymbolExpr* expr)
{
    symbol* sym = expr->sym();
    switch (sym->ident) {
        case iARRAY:
        case iREFARRAY:
            __ address(sym, sPRI);
            break;
        case iFUNCTN:
            assert(!sym->native);
            assert(sym->used());
            assert(sym->usage & uREAD);
            __ emit(OP_CONST_PRI, &sym->function()->funcid);
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
CodeGenerator::EmitIndexExpr(IndexExpr* expr)
{
    EmitExpr(expr->base());

    symbol* sym = expr->base()->val().sym;
    assert(sym);

    bool magic_string = (sym->tag == pc_tag_string && sym->dim.array.level == 0);

    const auto& idxval = expr->index()->val();
    if (idxval.ident == iCONSTEXPR) {
        if (!magic_string) {
            /* normal array index */
            if (idxval.constval != 0) {
                /* don't add offsets for zero subscripts */
                __ emit(OP_ADD_C, idxval.constval << 2);
            }
        } else {
            /* character index */
            if (idxval.constval != 0) {
                /* don't add offsets for zero subscripts */
                __ emit(OP_ADD_C, idxval.constval); /* 8-bit character */
            }
        }
    } else {
        __ emit(OP_PUSH_PRI);
        EmitExpr(expr->index());

        if (sym->dim.array.length != 0) {
            __ emit(OP_BOUNDS, sym->dim.array.length - 1); /* run time check for array bounds */
        } else {
            // vm uses unsigned compare, this protects against negative indices.
            __ emit(OP_BOUNDS, INT_MAX); 
        }

        if (magic_string) {
            __ emit(OP_POP_ALT);
            __ emit(OP_ADD);
        } else {
            __ emit(OP_POP_ALT);
            __ emit(OP_IDXADDR);
        }
    }

    // The indexed item is another array (multi-dimensional arrays).
    if (sym->dim.array.level > 0)
        __ emit(OP_LOAD_I);
}

void
CodeGenerator::EmitFieldAccessExpr(FieldAccessExpr* expr)
{
    assert(expr->token() == '.');

    // Note that we do not load an iACCESSOR here, we only make sure the base
    // is computed. Emit() never performs loads on l-values, that ability is
    // reserved for RvalueExpr().
    EmitExpr(expr->base());

    if (expr->field() && expr->field()->addr()) {
        __ const_alt(expr->field()->addr() << 2);
        __ emit(OP_ADD);
    }
}

void
CodeGenerator::EmitCallExpr(CallExpr* call)
{
    auto& val = call->val();

    // If returning an array, push a hidden parameter.
    if (val.sym) {
        cell retsize = CalcArraySize(val.sym);

        if (retsize)
            __ emit(OP_HEAP, retsize * sizeof(cell));
        __ emit(OP_PUSH_ALT);
        markheap(MEMUSE_STATIC, retsize, AllocScopeKind::Temp);
    }

    const auto& argv = call->argv();
    for (size_t i = argv.size() - 1; i < argv.size(); i--) {
        const auto& expr = argv[i].expr;
        const auto& arg = argv[i].arg;

        EmitExpr(expr);

        if (expr->as<DefaultArgExpr>()) {
            __ emit(OP_PUSH_PRI);
            continue;
        }

        const auto& val = expr->val();
        bool lvalue = expr->lvalue();

        switch (arg->type.ident) {
            case iVARARGS:
                if (val.ident == iVARIABLE || val.ident == iREFERENCE) {
                    assert(val.sym);
                    assert(lvalue);
                    /* treat a "const" variable passed to a function with a non-const
                     * "variable argument list" as a constant here */
                    if (val.sym->is_const && !arg->type.is_const) {
                        EmitRvalue(val);
                        __ setheap_pri();
                        markheap(MEMUSE_STATIC, 1, AllocScopeKind::Temp);
                    } else if (lvalue) {
                        __ address(val.sym, sPRI);
                    } else {
                        __ setheap_pri();
                        markheap(MEMUSE_STATIC, 1, AllocScopeKind::Temp);
                    }
                } else if (val.ident == iCONSTEXPR || val.ident == iEXPRESSION) {
                    /* allocate a cell on the heap and store the
                     * value (already in PRI) there */
                    __ setheap_pri();
                    markheap(MEMUSE_STATIC, 1, AllocScopeKind::Temp);
                }
                break;
            case iVARIABLE:
            case iREFARRAY:
                break;
            case iREFERENCE:
                if (val.ident == iVARIABLE || val.ident == iREFERENCE) {
                    assert(val.sym);
                    __ address(val.sym, sPRI);
                }
                break;
            default:
                assert(false);
                break;
        }

        __ emit(OP_PUSH_PRI);
    }

    EmitCall(call->sym(), argv.size());

    if (val.sym)
        __ emit(OP_POP_PRI);
}

void
CodeGenerator::EmitDefaultArgExpr(DefaultArgExpr* expr)
{
    const auto& arg = expr->arg();
    switch (arg->type.ident) {
        case iREFARRAY:
            EmitDefaultArray(arg);
            break;
        case iREFERENCE:
            __ const_pri(arg->def->val.get());
            __ setheap_pri();
            markheap(MEMUSE_STATIC, 1, AllocScopeKind::Temp);
            break;
        case iVARIABLE:
            __ const_pri(arg->def->val.get());
            break;
        default:
            assert(false);
    }

}

void
CodeGenerator::EmitCallUserOpExpr(CallUserOpExpr* expr)
{
    EmitExpr(expr->expr());

    const auto& userop = expr->userop();
    if (userop.oper) {
        auto val = expr->expr()->val();
        EmitUserOp(userop, &val);
    } else {
        EmitUserOp(userop, nullptr);
    }
}

void
CodeGenerator::EmitNewArrayExpr(NewArrayExpr* expr)
{
    int numdim = 0;
    auto& exprs = expr->exprs();
    const auto& type = expr->type();
    for (size_t i = 0; i < exprs.size(); i++) {
        EmitExpr(exprs[i]);

        if (i == exprs.size() - 1 && type.tag() == pc_tag_string)
            __ emit(OP_STRADJUST_PRI);

        __ emit(OP_PUSH_PRI);
        numdim++;
    }

    if (symbol* es = gTypes.find(type.tag())->asEnumStruct()) {
        // The last dimension is implicit in the size of the enum struct. Note
        // that when synthesizing a NewArrayExpr for old-style declarations,
        // it is impossible to have an enum struct.
        // :TODO: test this
        __ emit(OP_PUSH_C, es->addr());
        numdim++;
    }

    if (expr->autozero())
        __ emit(OP_GENARRAY_Z, numdim);
    else
        __ emit(OP_GENARRAY, numdim);
}

void
CodeGenerator::EmitIfStmt(IfStmt* stmt)
{
    Label flab1;

    EmitTest(stmt->cond(), false, &flab1);
    EmitStmt(stmt->on_true());
    if (stmt->on_false()) {
        Label flab2;
        if (!stmt->on_true()->IsTerminal()) {
            __ emit(OP_JUMP, &flab2);
        }
        __ bind(&flab1);
        EmitStmt(stmt->on_false());
        if (flab2.used())
            __ bind(&flab2);
    } else {
        __ bind(&flab1);
    }
}

void
CodeGenerator::EmitReturnArrayStmt(ReturnStmt* stmt)
{
    ArrayData array;
    BuildArrayInitializer(stmt->array(), nullptr, &array);

    if (array.iv.empty()) {
        symbol* sub = func_->array_return();

        // A much simpler copy can be emitted.
        __ load_hidden_arg(func_, sub, true);

        cell size = sub->dim.array.length;
        if (sub->tag == pc_tag_string)
            size = char_array_cells(size);

        __ emit(OP_MOVS, size * sizeof(cell));
        return;
    }

    auto fun = func_->function();
    if (!fun->array) {
        fun->array = new ArrayData;
        *fun->array = std::move(array);

        // No initializer == no data.
        assert(fun->array->data.empty());
        assert(fun->array->zeroes);

        cell iv_size = (cell)fun->array->iv.size();
        cell dat_addr = data_.dat_address();
        data_.Add(std::move(fun->array->iv));

        fun->array->iv.emplace_back(iv_size);
        fun->array->data.emplace_back(dat_addr);
    }

    cell dat_addr = fun->array->data[0];
    cell iv_size = fun->array->iv[0];
    assert(iv_size);
    assert(fun->array->zeroes);

    // push.pri                 ; save array expression result
    // alt = hidden array
    // initarray.alt            ; initialize IV (if needed)
    // move.pri
    // add.c <iv-size * 4>      ; address to data
    // move.alt
    // pop.pri
    // add.c <iv-size * 4>      ; address to data
    // memcopy <data-size>
    __ emit(OP_PUSH_PRI);
    __ load_hidden_arg(func_, func_->array_return(), false);
    __ emit(OP_INITARRAY_ALT, dat_addr, iv_size, 0, 0, 0);
    __ emit(OP_MOVE_PRI);
    __ emit(OP_ADD_C, iv_size * sizeof(cell));
    __ emit(OP_MOVE_ALT);
    __ emit(OP_POP_PRI);
    __ emit(OP_ADD_C, iv_size * sizeof(cell));
    __ emit(OP_MOVS, fun->array->zeroes * sizeof(cell));
}

void
CodeGenerator::EmitReturnStmt(ReturnStmt* stmt)
{
    if (stmt->expr()) {
        EmitExpr(stmt->expr());

        const auto& v = stmt->expr()->val();
        if (v.ident == iARRAY || v.ident == iREFARRAY)
            EmitReturnArrayStmt(stmt);
    } else {
        /* this return statement contains no expression */
        __ const_pri(0);
    }

    genheapfree(-1);
    genstackfree(-1); /* free everything on the stack */
    __ emit(OP_RETN);
}

void
CodeGenerator::EmitDeleteStmt(DeleteStmt* stmt)
{
    Expr* expr = stmt->expr();
    auto v = expr->val();

    // Only zap non-const lvalues.
    bool zap = expr->lvalue();
    if (zap && v.sym && v.sym->is_const)
        zap = false;

    EmitExpr(expr);

    bool popaddr = false;
    methodmap_method_t* accessor = nullptr;
    if (expr->lvalue()) {
        if (zap) {
            switch (v.ident) {
                case iACCESSOR:
                    // EmitRvalue() removes iACCESSOR so we store it locally.
                    accessor = v.accessor;
                    if (!accessor->setter) {
                        zap = false;
                        break;
                    }
                    __ emit(OP_PUSH_PRI);
                    popaddr = true;
                    break;
                case iARRAYCELL:
                case iARRAYCHAR:
                    __ emit(OP_PUSH_PRI);
                    popaddr = true;
                    break;
            }
        }

        EmitRvalue(&v);
    }

    // push.pri
    // push.c 1
    // sysreq.c N 1
    // stack 8
    __ emit(OP_PUSH_PRI);
    EmitCall(stmt->map()->dtor->target, 1);

    if (zap) {
        if (popaddr)
            __ emit(OP_POP_ALT);

        // Store 0 back.
        __ const_pri(0);
        if (accessor)
            InvokeSetter(accessor, FALSE);
        else
            EmitStore(&v);
    }
}

void
CodeGenerator::EmitRvalue(value* lval)
{
    switch (lval->ident) {
        case iARRAYCELL:
            __ emit(OP_LOAD_I);
            break;
        case iARRAYCHAR:
            __ emit(OP_LODB_I, 1);
            break;
        case iREFERENCE:
            assert(lval->sym);
            assert(lval->sym->vclass == sLOCAL || lval->sym->vclass == sARGUMENT);
            __ emit(OP_LREF_S_PRI, lval->sym->addr());
            break;
        case iACCESSOR:
            InvokeGetter(lval->accessor);
            lval->ident = iEXPRESSION;
            lval->accessor = nullptr;
            break;
        default:
            assert(lval->sym);
            if (lval->sym->vclass == sLOCAL || lval->sym->vclass == sARGUMENT)
              __ emit(OP_LOAD_S_PRI, lval->sym->addr());
            else
              __ emit(OP_LOAD_PRI, lval->sym->addr());
            break;
    }
}

void
CodeGenerator::EmitStore(const value* lval)
{
    switch (lval->ident) {
        case iARRAYCELL:
            __ emit(OP_STOR_I);
            break;
        case iARRAYCHAR:
            __ emit(OP_STRB_I, 1);
            break;
        case iREFERENCE:
            assert(lval->sym);
            assert(lval->sym->vclass == sLOCAL || lval->sym->vclass == sARGUMENT);
            __ emit(OP_SREF_S_PRI, lval->sym->addr());
            break;
        case iACCESSOR:
            InvokeSetter(lval->accessor, true);
            break;
        default:
            assert(lval->sym);
            if (lval->sym->vclass == sLOCAL || lval->sym->vclass == sARGUMENT)
                __ emit(OP_STOR_S_PRI, lval->sym->addr());
            else
                __ emit(OP_STOR_PRI, lval->sym->addr());
            break;
    }
}

void
CodeGenerator::InvokeGetter(methodmap_method_t* method)
{
    assert(method->getter);

    __ emit(OP_PUSH_PRI);
    EmitCall(method->getter, 1);
}

void
CodeGenerator::InvokeSetter(methodmap_method_t* method, bool save_pri)
{
    assert(method->setter);

    if (save_pri)
      __ emit(OP_PUSH_PRI);
    __ emit(OP_PUSH_PRI);
    __ emit(OP_PUSH_ALT);
    EmitCall (method->setter, 2);
    if (save_pri)
      __ emit(OP_POP_PRI);
}

void
CodeGenerator::EmitDoWhileStmt(DoWhileStmt* stmt)
{
    int token = stmt->token();
    assert(token == tDO || token == tWHILE);

    LoopContext loop_cx;
    loop_cx.stack_scope_id = stack_scope_id();
    loop_cx.heap_scope_id = heap_scope_id();
    ke::SaveAndSet<LoopContext*> push_context(&loop_, &loop_cx);

    __ bind(&loop_cx.continue_to);

    auto body = stmt->body();
    auto cond = stmt->cond();
    if (token == tDO) {
        EmitStmt(body);
        if (!body->IsTerminal())
            EmitTest(cond, true, &loop_cx.continue_to);
    } else {
        EmitTest(cond, false, &loop_cx.break_to);
        EmitStmt(body);
        if (!body->IsTerminal())
            __ emit(OP_JUMP, &loop_cx.continue_to);
    }

    __ bind(&loop_cx.break_to);
}

void
CodeGenerator::EmitLoopControlStmt(LoopControlStmt* stmt)
{
    int token = stmt->token();

    assert(loop_);
    assert(token == tBREAK || token == tCONTINUE);

    genstackfree(loop_->stack_scope_id);
    genheapfree(loop_->heap_scope_id);

    if (token == tBREAK)
        __ emit(OP_JUMP, &loop_->break_to);
    else
        __ emit(OP_JUMP, &loop_->continue_to);
}

void
CodeGenerator::EmitForStmt(ForStmt* stmt)
{
    auto scope = stmt->scope();
    if (scope) {
        pushstacklist();
        pushheaplist();
    }

    auto init = stmt->init();
    if (init)
        EmitStmt(init);

    LoopContext loop_cx;
    loop_cx.stack_scope_id = stack_scope_id();
    loop_cx.heap_scope_id = heap_scope_id();
    ke::SaveAndSet<LoopContext*> push_context(&loop_, &loop_cx);

    auto body = stmt->body();
    bool body_always_exits = body->flow_type() == Flow_Return ||
                             body->flow_type() == Flow_Break;

    auto advance = stmt->advance();
    auto cond = stmt->cond();
    if (advance && !stmt->never_taken()) {
        // top:
        //   <cond>
        //   jf break
        //   <body>
        // continue:
        //   <advance>
        //   jmp top
        // break:
        Label top;
        __ bind(&top);

        if (cond && !stmt->always_taken())
            EmitTest(cond, false, &loop_cx.break_to);

        EmitStmt(body);

        if (stmt->has_continue()) {
            __ bind(&loop_cx.continue_to);
            EmitExpr(advance);
        }
        if (!body_always_exits)
            __ emit(OP_JUMP, &top);
    } else if (!stmt->never_taken()) {
        // continue:
        //   <cond>
        //   jf break
        //   <body>
        //   jmp continue
        // break:
        __ bind(&loop_cx.continue_to);

        if (cond && !stmt->always_taken())
            EmitTest(cond, false, &loop_cx.break_to);

        EmitStmt(body);

        if (!body_always_exits)
            __ emit(OP_JUMP, &loop_cx.continue_to);
    }
    __ bind(&loop_cx.break_to);

    if (scope) {
        popheaplist(true);
        popstacklist(true);
    }
}

void
CodeGenerator::EmitSwitchStmt(SwitchStmt* stmt)
{
    EmitExpr(stmt->expr());

    Label exit_label;
    Label table_label;
    __ emit(OP_SWITCH, &table_label);

    // Note: we use map for ordering so the case table is sorted.
    std::map<cell, Label> case_labels;

    for (const auto& case_entry : stmt->cases()) {
        Stmt* stmt = case_entry.second;

        Label label;
        __ bind(&label);
        for (const auto& expr : case_entry.first) {
            const auto& v = expr->val();
            assert(v.ident == iCONSTEXPR);

            case_labels.emplace(v.constval, label);
        }

        EmitStmt(stmt);
        if (!stmt->IsTerminal())
            __ emit(OP_JUMP, &exit_label);
    }

    Label default_label;
    Label* defcase = &exit_label;
    if (stmt->default_case()) {
        __ bind(&default_label);

        EmitStmt(stmt->default_case());
        if (!stmt->default_case()->IsTerminal())
            __ emit(OP_JUMP, &exit_label);

        defcase = &default_label;
    }

    __ bind(&table_label);
    __ casetbl((int)case_labels.size(), defcase);

    for (auto& pair : case_labels)
        __ casetbl_entry(pair.first, &pair.second);

    __ bind(&exit_label);
}

void
CodeGenerator::EmitFunctionInfo(FunctionInfo* info)
{
    ke::SaveAndSet<symbol*> set_func(&func_, info->sym());

    pc_max_func_memory = 0;
    pc_current_memory = 0;

    if (info->sym()->unused())
        return;

    cc_.functions().emplace(info->sym());

    if (!info->body())
        return;

    __ bind(&info->sym()->function()->label);
    __ emit(OP_PROC);
    AddDebugLine(info->pos().line);
    EmitBreak();
    pc_current_stack = 0;

    EmitStmt(info->body());

    assert(!has_stack_or_heap_scopes());

    // If return keyword is missing, we added it in the semantic pass.
    __ emit(OP_ENDPROC);

    stack_scopes_.clear();
    heap_scopes_.clear();

    info->sym()->setAddr(info->sym()->function()->label.offset());
    info->sym()->codeaddr = asm_.pc();

    pc_max_memory = std::max(pc_max_func_memory, pc_max_memory);
}

void
CodeGenerator::EmitBreak()
{
    if (last_break_op_ && *last_break_op_ == asm_.position())
        return;
    __ emit(OP_BREAK);
    last_break_op_.init(asm_.position());
}

void
CodeGenerator::EmitEnumStructDecl(EnumStructDecl* decl)
{
    for (const auto& fun : decl->methods())
        EmitFunctionInfo(fun->info());
}

void
CodeGenerator::EmitMethodmapDecl(MethodmapDecl* decl)
{
    for (const auto& prop : decl->properties()) {
        if (prop->getter)
            EmitFunctionInfo(prop->getter);
        if (prop->setter)
            EmitFunctionInfo(prop->setter);
    }
    for (const auto& method : decl->methods())
        EmitFunctionInfo(method->decl->info());
}

void
CodeGenerator::EmitCall(symbol* sym, cell nargs)
{
    assert(sym->used());

    if (sym->native) {
        if (auto alias = sym->function()->alias) {
            sym = alias;
            assert(sym->used());
        }

        if (sym->addr() < 0) {
            sym->setAddr((cell)native_list_.size());
            native_list_.emplace_back(sym);
        }
        __ emit(OP_SYSREQ_N, sym->addr(), nargs);
    } else {
        __ emit(OP_PUSH_C, nargs);
        __ emit(OP_CALL, &sym->function()->label);
    }
}

void
CodeGenerator::EmitDefaultArray(arginfo* arg)
{
    DefaultArg* def = arg->def;
    if (!def->val) {
        def->val = ke::Some(data_.dat_address());

        // Make copies since we cache DefaultArgs across compilations. This
        // will go away when the two-pass parser dies.
        PoolList<cell> iv = def->array->iv;
        PoolList<cell> data = def->array->data;

        data_.Add(std::move(iv));
        data_.Add(std::move(data));
        data_.AddZeroes(def->array->zeroes);
    }

    if (arg->type.is_const || !def->array) {
        // No modification is possible, so use the array we emitted. (This is
        // why we emitted the zeroes above.)
        __ const_pri(def->val.get());
    } else {
        cell iv_size = (cell)def->array->iv.size();
        cell data_size = (cell)def->array->data.size();
        cell total_size = iv_size + data_size + def->array->zeroes;

        //  heap <size>
        //  move.alt        ; pri = new address
        //  init.array
        //  move.alt        ; pri = new address
        __ emit(OP_HEAP, total_size * sizeof(cell));
        __ emit(OP_INITARRAY_ALT, def->val.get(), iv_size, data_size, def->array->zeroes, 0);
        __ emit(OP_MOVE_PRI);
        markheap(MEMUSE_STATIC, total_size, AllocScopeKind::Temp);
    }
}

void
CodeGenerator::EmitUserOp(const UserOperation& user_op, value* lval)
{
    // for increment and decrement operators, the symbol must first be loaded
    // (and stored back afterwards)
    if (user_op.oper == tINC || user_op.oper == tDEC) {
        assert(!user_op.savepri);
        assert(lval != NULL);
        if (lval->ident == iARRAYCELL || lval->ident == iARRAYCHAR)
            __ emit(OP_PUSH_PRI);
        if (lval->ident != iACCESSOR)
            EmitRvalue(lval); /* get the symbol's value in PRI */
    }

    assert(!user_op.savepri || !user_op.savealt); /* either one MAY be set, but not both */
    if (user_op.savepri) {
        // the chained comparison operators require that the ALT register is
        // unmodified, so we save it here; actually, we save PRI because the normal
        // instruction sequence (without user operator) swaps PRI and ALT
        __ emit(OP_PUSH_PRI);
    } else if (user_op.savealt) {
        /* for the assignment operator, ALT may contain an address at which the
         * result must be stored; this address must be preserved accross the
         * call
         */
        assert(lval != NULL); /* this was checked earlier */
        assert(lval->ident == iARRAYCELL || lval->ident == iARRAYCHAR); /* checked earlier */
        __ emit(OP_PUSH_ALT);
    }

    /* push parameters, call the function */
    switch (user_op.paramspassed) {
        case 1:
            __ emit(OP_PUSH_PRI);
            break;
        case 2:
            /* note that 1) a function expects that the parameters are pushed
             * in reversed order, and 2) the left operand is in the secondary register
             * and the right operand is in the primary register */
            if (user_op.swapparams) {
                __ emit(OP_PUSH_ALT);
                __ emit(OP_PUSH_PRI);
            } else {
                __ emit(OP_PUSH_PRI);
                __ emit(OP_PUSH_ALT);
            }
            break;
        default:
            assert(0);
    }
    assert(user_op.sym->ident == iFUNCTN);
    EmitCall(user_op.sym, user_op.paramspassed);

    if (user_op.savepri || user_op.savealt)
        __ emit(OP_POP_ALT); /* restore the saved PRI/ALT that into ALT */
    if (user_op.oper == tINC || user_op.oper == tDEC) {
        assert(lval != NULL);
        if (lval->ident == iARRAYCELL || lval->ident == iARRAYCHAR)
            __ emit(OP_POP_ALT); /* restore address (in ALT) */
        if (lval->ident != iACCESSOR) {
            EmitStore(lval); /* store PRI in the symbol */
            __ emit(OP_MOVE_PRI);
        }
    }
}

void CodeGenerator::EmitInc(const value* lval)
{
    switch (lval->ident) {
        case iARRAYCELL:
            __ emit(OP_INC_I);
            break;
        case iARRAYCHAR:
            __ emit(OP_PUSH_PRI);
            __ emit(OP_PUSH_ALT);
            __ emit(OP_MOVE_ALT);
            __ emit(OP_LODB_I, 1);
            __ emit(OP_INC_PRI);
            __ emit(OP_STRB_I, 1);
            __ emit(OP_POP_ALT);
            __ emit(OP_POP_PRI);
            break;
        case iREFERENCE:
            __ emit(OP_PUSH_PRI);
            __ emit(OP_LREF_S_PRI, lval->sym->addr());
            __ emit(OP_INC_PRI);
            __ emit(OP_SREF_S_PRI, lval->sym->addr());
            __ emit(OP_POP_PRI);
            break;
        case iACCESSOR:
            __ emit(OP_INC_PRI);
            InvokeSetter(lval->accessor, false);
            break;
        default:
            if (lval->sym->vclass == sLOCAL || lval->sym->vclass == sARGUMENT)
                __ emit(OP_INC_S, lval->sym->addr());
            else
                __ emit(OP_INC, lval->sym->addr());
            break;
    }
}

void CodeGenerator::EmitDec(const value* lval)
{
    switch (lval->ident) {
        case iARRAYCELL:
            __ emit(OP_DEC_I);
            break;
        case iARRAYCHAR:
            __ emit(OP_PUSH_PRI);
            __ emit(OP_PUSH_ALT);
            __ emit(OP_MOVE_ALT);
            __ emit(OP_LODB_I, 1);
            __ emit(OP_DEC_PRI);
            __ emit(OP_STRB_I, 1);
            __ emit(OP_POP_ALT);
            __ emit(OP_POP_PRI);
            break;
        case iREFERENCE:
            __ emit(OP_PUSH_PRI);
            __ emit(OP_LREF_S_PRI, lval->sym->addr());
            __ emit(OP_DEC_PRI);
            __ emit(OP_SREF_S_PRI, lval->sym->addr());
            __ emit(OP_POP_PRI);
            break;
        case iACCESSOR:
            __ emit(OP_DEC_PRI);
            InvokeSetter(lval->accessor, false);
            break;
        default:
            if (lval->sym->vclass == sLOCAL || lval->sym->vclass == sARGUMENT)
                __ emit(OP_DEC_S, lval->sym->addr());
            else
                __ emit(OP_DEC, lval->sym->addr());
            break;
    }
}

void
CodeGenerator::EnterMemoryScope(std::vector<MemoryScope>& frame, AllocScopeKind kind)
{
    if (frame.empty())
        frame.push_back(MemoryScope{0, kind});
    else
        frame.push_back(MemoryScope{frame.back().scope_id + 1, kind});
}

void
CodeGenerator::AllocInScope(MemoryScope& scope, int type, int size)
{
    if (type == MEMUSE_STATIC && !scope.usage.empty() && scope.usage.back().type == MEMUSE_STATIC) {
        scope.usage.back().size += size;
    } else {
        scope.usage.push_back(MemoryUse{type, size});
    }

    pc_current_memory += size;
    pc_max_memory = std::max(pc_current_memory, pc_max_memory);
}

int
CodeGenerator::PopScope(std::vector<MemoryScope>& scope_list)
{
    MemoryScope scope = ke::PopBack(&scope_list);
    int total_use = 0;
    while (!scope.usage.empty()) {
        assert(scope.usage.back().size <= pc_current_memory);
        total_use += scope.usage.back().size;
        scope.usage.pop_back();
    }
    pc_current_memory -= total_use;
    return total_use;
}

void
CodeGenerator::pushheaplist(AllocScopeKind kind)
{
    EnterMemoryScope(heap_scopes_, kind);
}

// Sums up array usage in the current heap tracer and convert it into a dynamic array.
// This is used for the ternary operator, which needs to convert its array usage into
// something dynamically managed.
// !Note:
// This might break if expressions can ever return dynamic arrays.
// Thus, we assert() if something is non-static here.
// Right now, this poses no problem because this type of expression is impossible:
//   (a() ? return_array() : return_array()) ? return_array() : return_array()
cell_t
CodeGenerator::pop_static_heaplist()
{
    cell_t total = 0;
    for (const auto& use : heap_scopes_.back().usage) {
        assert(use.type == MEMUSE_STATIC);
        total += use.size;
    }
    PopScope(heap_scopes_);
    return total;
}

int
CodeGenerator::markheap(int type, int size, AllocScopeKind kind)
{
    MemoryScope* scope = nullptr;
    if (kind == AllocScopeKind::Temp) {
        // Every expression should have an immediate temporary scope.
        scope = &heap_scopes_.back();
        assert(scope->kind == AllocScopeKind::Temp);
        assert(!scope->blacklisted);
    } else {
        // Declarations will have an immediate temporary scope as well, but we
        // must allocate into the normal scope before using the temporary one
        // (because it's a LIFO allocation).
        for (auto iter = heap_scopes_.rbegin(); iter != heap_scopes_.rend(); iter++) {
            if (iter->kind == AllocScopeKind::Temp) {
                assert(iter->usage.empty());
                iter->blacklisted = true;
                continue;
            }
            if (iter->kind == AllocScopeKind::Normal) {
                scope = &*iter;
                break;
            }
        }
        assert(scope);
    }

    AllocInScope(*scope, type, size);
    return size;
}

void
CodeGenerator::pushstacklist()
{
    EnterMemoryScope(stack_scopes_, AllocScopeKind::Normal);
}

int
CodeGenerator::markstack(int type, int size)
{
    pc_current_stack += size;
    AllocInScope(stack_scopes_.back(), type, size);
    return size;
}

// Generates code to free all heap allocations on a tracker
void
CodeGenerator::modheap_for_scope(const MemoryScope& scope)
{
    for (size_t i = scope.usage.size() - 1; i < scope.usage.size(); i--) {
        const MemoryUse& use = scope.usage[i];
        if (use.type == MEMUSE_STATIC) {
            if (use.size)
                __ emit(OP_HEAP, (-1) * use.size * sizeof(cell));
        } else {
            __ emit(OP_TRACKER_POP_SETHEAP);
        }
    }
}

void
CodeGenerator::modstk_for_scope(const MemoryScope& scope)
{
    cell_t total = 0;
    for (const auto& use : scope.usage) {
        assert(use.type == MEMUSE_STATIC);
        total += use.size;
    }
    if (total)
        __ emit(OP_STACK, total * sizeof(cell));
}

void
CodeGenerator::popheaplist(bool codegen)
{
    assert(!heap_scopes_.empty());
    if (codegen)
        modheap_for_scope(heap_scopes_.back());
    PopScope(heap_scopes_);
}

void
CodeGenerator::genstackfree(int stop_id)
{
    for (size_t i = stack_scopes_.size() - 1; i < stack_scopes_.size(); i--) {
        const MemoryScope& scope = stack_scopes_[i];
        if (scope.scope_id <= stop_id)
            break;
        modstk_for_scope(scope);
    }
}

void
CodeGenerator::genheapfree(int stop_id)
{
    for (size_t i = heap_scopes_.size() - 1; i < heap_scopes_.size(); i--) {
        const MemoryScope& scope = heap_scopes_[i];
        if (scope.scope_id <= stop_id)
            break;
        modheap_for_scope(scope);
    }
}

void
CodeGenerator::popstacklist(bool codegen)
{
    if (codegen)
        modstk_for_scope(stack_scopes_.back());
    pc_current_stack -= PopScope(stack_scopes_);
}

void
CodeGenerator::LinkPublicFunction(symbol* sym, uint32_t id)
{
    __ bind_to(&sym->function()->funcid, id);
}
