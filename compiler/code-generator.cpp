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
#include "emitter.h"
#include "errors.h"
#include "expressions.h"
#include "output-buffer.h"
#include "sctracker.h"
#include "symbols.h"

CodeGenerator::CodeGenerator(CompileContext& cc, ParseTree* tree)
  : cc_(cc),
    tree_(tree)
{
}

void
CodeGenerator::Generate()
{
    writeleader();
    EmitStmtList(tree_);
    writetrailer();

    //
    //assemble(cc, cg, binfname, compression_level);
}

void
CodeGenerator::AddDebugFile(const std::string& file)
{
    auto str = ke::StringPrintf("F:%x %s", code_idx, file.c_str());
    debug_strings_.emplace_back(std::move(str));
}

void
CodeGenerator::AddDebugLine(int linenr)
{
    auto str = ke::StringPrintf("L:%x %x", code_idx, linenr);
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
                                   code_idx, sym->ident, sym->vclass, (int)sym->is_const);
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
        setline(FALSE);
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
                ldconst(0, sPRI);
            ffabort(xEXIT);
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
            int flab1 = getlabel();
            EmitTest(s->expr(), true, flab1);
            ffabort(xASSERTION);
            setlabel(flab1);
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

    sym->codeaddr = code_idx;

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

    sym->setAddr(gDataQueue.dat_address());

    if (sym->ident == iVARIABLE) {
        assert(!init || init->right()->val().ident == iCONSTEXPR);
        if (init)
            gDataQueue.Add(init->right()->val().constval);
        else
            gDataQueue.Add(0);
    } else if (sym->ident == iARRAY) {
        ArrayData array;
        BuildArrayInitializer(decl, &array, gDataQueue.dat_address());

        gDataQueue.Add(std::move(array.iv));
        gDataQueue.Add(std::move(array.data));
        gDataQueue.AddZeroes(array.zeroes);
    } else {
        assert(false);
    }

    // Data queue is only purged in endfunc(), so make sure each global
    // dumps the data queue.
    if (sym->vclass == sGLOBAL)
        gDataQueue.Emit();
}

void
CodeGenerator::EmitLocalVar(VarDecl* decl)
{
    symbol* sym = decl->sym();
    BinaryExpr* init = decl->init();
    sp::Atom* name = decl->name();

    if (sym->ident == iVARIABLE) {
        markstack(MEMUSE_STATIC, 1);
        sym->setAddr(-pc_current_stack * sizeof(cell));
        markexpr(sLDECL, name->chars(), sym->addr());

        if (init) {
            const auto& val = init->right()->val();
            if (val.ident == iCONSTEXPR) {
                pushval(val.constval);
            } else {
                EmitExpr(init->right());
                pushreg(sPRI);
            }
        } else {
            // Note: we no longer honor "decl" for scalars.
            pushval(0);
        }
    } else if (sym->ident == iARRAY) {
        ArrayData array;
        BuildArrayInitializer(decl, &array, 0);

        cell iv_size = array.iv.size();
        cell data_size = array.data.size() + array.zeroes;
        cell total_size = iv_size + data_size;

        markstack(MEMUSE_STATIC, total_size);
        sym->setAddr(-pc_current_stack * sizeof(cell));
        markexpr(sLDECL, name->chars(), sym->addr());
        modstk(-(total_size * sizeof(cell)));

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
        if (!decl->autozero() && fill_size && fill_value == 0)
            fill_size = 0;

        addr_reg(sym->addr(), sPRI);
        emit_initarray(sPRI, iv_addr, iv_size, non_filled, fill_size, fill_value);
    } else if (sym->ident == iREFARRAY) {
        // Note that genarray() pushes the address onto the stack, so we don't
        // need to call modstk() here.
        markheap(MEMUSE_DYNAMIC, 0, AllocScopeKind::Normal);
        markstack(MEMUSE_STATIC, 1);
        sym->setAddr(-pc_current_stack * sizeof(cell));

        markexpr(sLDECL, name->chars(), sym->addr());

        auto init_rhs = decl->init_rhs();
        if (NewArrayExpr* ctor = init_rhs->as<NewArrayExpr>()) {
            EmitExpr(ctor);
        } else if (StringExpr* ctor = init_rhs->as<StringExpr>()) {
            auto queue_size = gDataQueue.size();
            auto str_addr = gDataQueue.dat_address();
            gDataQueue.Add(ctor->text()->chars(), ctor->text()->length());

            auto cells = gDataQueue.size() - queue_size;
            assert(cells > 0);

            pushval(cells);
            genarray(1, decl->autozero());
            ldconst(str_addr, sPRI);
            copyarray(sym, cells * sizeof(cell));
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

    sym->codeaddr = code_idx;

    auto init = decl->init_rhs()->as<StructExpr>();
    for (const auto& field : init->fields()) {
        auto arg = pstructs_getarg(ps, field.name);
        if (auto expr = field.value->as<StringExpr>()) {
            values[arg->index] = gDataQueue.dat_address();
            gDataQueue.Add(expr->text()->chars(), expr->text()->length());
        } else if (auto expr = field.value->as<TaggedValueExpr>()) {
            values[arg->index] = expr->value();
        } else if (auto expr = field.value->as<SymbolExpr>()) {
            values[arg->index] = expr->sym()->addr();
        } else {
            assert(false);
        }
    }

    sym->setAddr(gDataQueue.dat_address());

    for (const auto& value : values)
        gDataQueue.Add(value);
    gDataQueue.Emit();
}

void
CodeGenerator::EmitExpr(Expr* expr)
{
    AutoErrorPos aep(expr->pos());

    if (expr->val().ident == iCONSTEXPR) {
        ldconst(expr->val().constval, sPRI);
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
            rvalue(&val);
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
                address(e->sym(), sPRI);
            break;
        }
        case AstKind::StringExpr: {
            auto se = expr->to<StringExpr>();
            auto addr = gDataQueue.dat_address();
            gDataQueue.Add(se->text()->chars(), se->text()->length());
            ldconst(addr, sPRI);
            break;
        }
        case AstKind::ArrayExpr: {
            auto e = expr->to<ArrayExpr>();
            auto addr = gDataQueue.dat_address();
            for (const auto& expr : e->exprs())
                gDataQueue.Add(expr->val().constval);
            ldconst(addr, sPRI);
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
CodeGenerator::EmitTest(Expr* expr, bool jump_on_true, int target)
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
        jmp_ne0(target);
    else
        jmp_eq0(target);
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

bool
CodeGenerator::EmitUnaryExprTest(UnaryExpr* expr, bool jump_on_true, int target)
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
                emit_userop(userop, &tmp);
            } else {
                if (expr->token() == tINC)
                    inc(&tmp); /* increase variable first */
                else
                    dec(&tmp);
            }
            rvalue(&tmp);  /* and read the result into PRI */
        } else {
            pushreg(sPRI);
            invoke_getter(val.accessor);
            if (userop.sym) {
                emit_userop(userop, &tmp);
            } else {
                if (expr->token() == tINC)
                    inc_pri();
                else
                    dec_pri();
            }
            popreg(sALT);
            invoke_setter(val.accessor, TRUE);
        }
    } else {
        if (val.ident == iARRAYCELL || val.ident == iARRAYCHAR || val.ident == iACCESSOR) {
            // Save base address. Stack: [addr]
            pushreg(sPRI);
            // Get pre-inc value.
            rvalue(val);
            // Save pre-inc value, but swap its position with the address.
            popreg(sALT);       // Stack: []
            pushreg(sPRI);      // Stack: [val]
            if (userop.sym) {
                pushreg(sALT);      // Stack: [val addr]
                // Call the overload.
                pushreg(sPRI);
                markexpr(sPARM, nullptr, 0);
                ffcall(userop.sym, 1);
                // Restore the address and emit the store.
                popreg(sALT);       // Stack: [val]
                store(&val);
            } else {
                if (val.ident != iACCESSOR)
                    moveto1();
                if (expr->token() == tINC)
                    inc(&val);
                else
                    dec(&val);
            }
            popreg(sPRI);
        } else {
            // Much simpler case when we don't have to save the base address.
            rvalue(val);
            pushreg(sPRI);
            if (userop.sym) {
                pushreg(sPRI);
                markexpr(sPARM, nullptr, 0);
                ffcall(userop.sym, 1);
                store(&val);
            } else {
                if (expr->token() == tINC)
                    inc(&val);
                else
                    dec(&val);
            }
            popreg(sPRI);
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
                    pushreg(sPRI);
                    rvalue(left_val);
                    saved_lhs = true;
                }
                break;
            case iACCESSOR:
                pushreg(sPRI);
                if (oper)
                    rvalue(left_val);
                saved_lhs = true;
                break;
            default:
                assert(left->lvalue());
                if (oper)
                    rvalue(left_val);
                break;
        }

        if (expr->array_copy_length()) {
            assert(!oper);
            assert(!expr->assignop().sym);

            pushreg(sPRI);
            EmitExpr(right);
            popreg(sALT);
            memcopy(expr->array_copy_length() * sizeof(cell));
            return;
        }
    }

    assert(!expr->array_copy_length());
    assert(left_val.ident != iARRAY && left_val.ident != iREFARRAY);

    EmitBinaryInner(oper, expr->userop(), left, right);

    if (IsAssignOp(token)) {
        if (saved_lhs)
            popreg(sALT);

        auto tmp = left_val;
        if (expr->assignop().sym)
            emit_userop(expr->assignop(), nullptr);
        store(&tmp);
    }
}

void
CodeGenerator::EmitBinaryInner(OpFunc oper, const UserOperation& in_user_op, Expr* left,
                               Expr* right)
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
            EmitExpr(right);
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
            EmitExpr(right);
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
CodeGenerator::EmitLogicalExpr(LogicalExpr* expr)
{
    bool jump_on_true = expr->token() == tlOR;

    int shortcircuit = getlabel();
    int done = getlabel();

    EmitTest(expr, jump_on_true, shortcircuit);
    ldconst(!jump_on_true, sPRI);
    jumplabel(done);
    setlabel(shortcircuit);
    ldconst(jump_on_true, sPRI);
    setlabel(done);
}

void
CodeGenerator::EmitLogicalExprTest(LogicalExpr* root, bool jump_on_true, int target)
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

    int fallthrough = getlabel();
    for (size_t i = 0; i < sequence.size() - 1; i++) {
        auto expr = sequence.at(i);
        if (root->token() == tlOR) {
            if (jump_on_true)
                EmitTest(expr, true, target);
            else
                EmitTest(expr, true, fallthrough);
        } else {
            assert(root->token() == tlAND);
            if (jump_on_true)
                EmitTest(expr, false, fallthrough);
            else
                EmitTest(expr, false, target);
        }
    }

    Expr* last = sequence.back();
    EmitTest(last, jump_on_true, target);
    setlabel(fallthrough);
}

bool
CodeGenerator::EmitChainedCompareExprTest(ChainedCompareExpr* root, bool jump_on_true, int target)
{
    // No optimization for user operators or for compare chains.
    if (root->ops().size() > 1 || root->ops()[0].userop.sym)
        return false;

    Expr* left = root->first();
    Expr* right = root->ops()[0].expr;

    pushheaplist(AllocScopeKind::Temp);

    EmitExpr(right);
    pushreg(sPRI);
    EmitExpr(left);
    popreg(sALT);

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
    jumplabel_cond(token, target);
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
        // emit_userop implicitly guarantees this, as do os_less etc which
        // use XCHG to swap the LHS/RHS expressions.
        if (count)
            relop_prefix();
        EmitBinaryInner(op.oper, op.userop, left, op.expr);
        if (count)
            relop_suffix();

        left = op.expr;
        count++;
    }
}

void
CodeGenerator::EmitTernaryExpr(TernaryExpr* expr)
{
    AutoStage stage;

    ke::Maybe<cell_t> branch1, branch2;
    EmitTernaryInner(expr, &branch1, &branch2);

    if (branch1.isValid() != branch2.isValid()) {
        stage.Rewind();

        // Try again, this time make sure both branches have a tracker.push.c.
        EmitTernaryInner(expr, &branch1, &branch2);
    }
    assert(branch1.isValid() == branch2.isValid());

    if (branch1.isValid() && branch2.isValid())
        markheap(MEMUSE_DYNAMIC, 0, AllocScopeKind::Temp);
}

void
CodeGenerator::EmitTernaryInner(TernaryExpr* expr, ke::Maybe<cell_t>* branch1,
                                ke::Maybe<cell_t>* branch2)
{
    EmitExpr(expr->first());

    int flab1 = getlabel();
    int flab2 = getlabel();

    pushheaplist(AllocScopeKind::Temp);
    jmp_eq0(flab1); /* go to second expression if primary register==0 */

    EmitExpr(expr->second());

    auto total1 = pop_static_heaplist();
    if (total1 || branch2->isValid()) {
        setheap_save(total1 * sizeof(cell));
        branch1->init(total1);
    }

    pushheaplist(AllocScopeKind::Temp);
    jumplabel(flab2);
    setlabel(flab1);

    EmitExpr(expr->third());

    auto total2 = pop_static_heaplist();
    if (total2 || branch1->isValid()) {
        setheap_save(total2 * sizeof(cell));
        branch2->init(total2);
    }
    setlabel(flab2);
}

void
CodeGenerator::EmitSymbolExpr(SymbolExpr* expr)
{
    symbol* sym = expr->sym();
    switch (sym->ident) {
        case iARRAY:
        case iREFARRAY:
            address(sym, sPRI);
            break;
        case iFUNCTN:
            load_glbfn(sym);
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
        EmitExpr(expr->index());

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
CodeGenerator::EmitFieldAccessExpr(FieldAccessExpr* expr)
{
    assert(expr->token() == '.');

    // Note that we do not load an iACCESSOR here, we only make sure the base
    // is computed. Emit() never performs loads on l-values, that ability is
    // reserved for RvalueExpr().
    EmitExpr(expr->base());

    if (expr->field() && expr->field()->addr()) {
        ldconst(expr->field()->addr() << 2, sALT);
        ob_add();
    }
}

void
CodeGenerator::EmitCallExpr(CallExpr* call)
{
    auto& val = call->val();

    // If returning an array, push a hidden parameter.
    if (val.sym) {
        cell retsize = CalcArraySize(val.sym);

        modheap(retsize * sizeof(cell));
        pushreg(sALT);
        markheap(MEMUSE_STATIC, retsize, AllocScopeKind::Temp);
    }

    const auto& argv = call->argv();
    for (size_t i = argv.size() - 1; i < argv.size(); i--) {
        const auto& expr = argv[i].expr;
        const auto& arg = argv[i].arg;

        EmitExpr(expr);

        if (expr->as<DefaultArgExpr>()) {
            pushreg(sPRI);
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

    ffcall(call->sym(), argv.size());

    if (val.sym)
        popreg(sPRI); // Pop hidden parameter as function result
}

void
CodeGenerator::EmitDefaultArgExpr(DefaultArgExpr* expr)
{
    const auto& arg = expr->arg();
    switch (arg->type.ident) {
        case iREFARRAY:
            emit_default_array(arg);
            break;
        case iREFERENCE:
            setheap(arg->def->val.get());
            break;
        case iVARIABLE:
            ldconst(arg->def->val.get(), sPRI);
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
        emit_userop(userop, &val);
    } else {
        emit_userop(userop, nullptr);
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
            stradjust(sPRI);

        pushreg(sPRI);
        numdim++;
    }

    if (symbol* es = gTypes.find(type.tag())->asEnumStruct()) {
        // The last dimension is implicit in the size of the enum struct. Note
        // that when synthesizing a NewArrayExpr for old-style declarations,
        // it is impossible to have an enum struct.
        // :TODO: test this
        pushval(es->addr());
        numdim++;
    }

    genarray(numdim, expr->autozero());
}

void
CodeGenerator::EmitIfStmt(IfStmt* stmt)
{
    int flab1 = getlabel();

    EmitTest(stmt->cond(), false, flab1);
    EmitStmt(stmt->on_true());
    if (stmt->on_false()) {
        ke::Maybe<int> flab2;
        if (!stmt->on_true()->IsTerminal()) {
            flab2.init(getlabel());
            jumplabel(*flab2);
        }
        setlabel(flab1);
        EmitStmt(stmt->on_false());
        if (flab2)
            setlabel(*flab2);
    } else {
        setlabel(flab1);
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
        load_hidden_arg(func_, sub, true);

        cell size = sub->dim.array.length;
        if (sub->tag == pc_tag_string)
            size = char_array_cells(size);

        memcopy(size * sizeof(cell));
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
        cell dat_addr = gDataQueue.dat_address();
        gDataQueue.Add(std::move(fun->array->iv));

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
    pushreg(sPRI);
    load_hidden_arg(func_, func_->array_return(), false);
    emit_initarray(sALT, dat_addr, iv_size, 0, 0, 0);
    moveto1();
    addconst(iv_size * sizeof(cell));
    move_alt();
    popreg(sPRI);
    addconst(iv_size * sizeof(cell));
    memcopy(fun->array->zeroes * sizeof(cell));
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
        ldconst(0, sPRI);
    }

    genheapfree(-1);
    genstackfree(-1); /* free everything on the stack */
    ffret();
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
                    // rvalue() removes iACCESSOR so we store it locally.
                    accessor = v.accessor;
                    if (!accessor->setter) {
                        zap = false;
                        break;
                    }
                    pushreg(sPRI);
                    popaddr = true;
                    break;
                case iARRAYCELL:
                case iARRAYCHAR:
                    pushreg(sPRI);
                    popaddr = true;
                    break;
            }
        }

        rvalue(&v);
    }

    // push.pri
    // push.c 1
    // sysreq.c N 1
    // stack 8
    pushreg(sPRI);
    ffcall(stmt->map()->dtor->target, 1);

    if (zap) {
        if (popaddr)
            popreg(sALT);

        // Store 0 back.
        ldconst(0, sPRI);
        if (accessor)
            invoke_setter(accessor, FALSE);
        else
            store(&v);
    }

    markexpr(sEXPR, NULL, 0);
}

struct LoopContext {
    int break_to;
    int continue_to;
    int stack_scope_id;
    int heap_scope_id;
};
LoopContext* sLoopContext = nullptr;

void
CodeGenerator::EmitDoWhileStmt(DoWhileStmt* stmt)
{
    int token = stmt->token();
    assert(token == tDO || token == tWHILE);

    LoopContext loop_cx;
    loop_cx.break_to = getlabel();
    loop_cx.continue_to = getlabel();
    loop_cx.stack_scope_id = stack_scope_id();
    loop_cx.heap_scope_id = heap_scope_id();
    ke::SaveAndSet<LoopContext*> push_context(&sLoopContext, &loop_cx);

    setlabel(loop_cx.continue_to);

    auto body = stmt->body();
    auto cond = stmt->cond();
    if (token == tDO) {
        EmitStmt(body);
        if (!body->IsTerminal())
            EmitTest(cond, true, loop_cx.continue_to);
    } else {
        EmitTest(cond, false, loop_cx.break_to);
        EmitStmt(body);
        if (!body->IsTerminal())
            jumplabel(loop_cx.continue_to);
    }

    setlabel(loop_cx.break_to);
}

void
CodeGenerator::EmitLoopControlStmt(LoopControlStmt* stmt)
{
    int token = stmt->token();

    assert(sLoopContext);
    assert(token == tBREAK || token == tCONTINUE);

    genstackfree(sLoopContext->stack_scope_id);
    genheapfree(sLoopContext->heap_scope_id);

    if (token == tBREAK)
        jumplabel(sLoopContext->break_to);
    else
        jumplabel(sLoopContext->continue_to);
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
    loop_cx.break_to = getlabel();
    loop_cx.continue_to = getlabel();
    loop_cx.stack_scope_id = stack_scope_id();
    loop_cx.heap_scope_id = heap_scope_id();
    ke::SaveAndSet<LoopContext*> push_context(&sLoopContext, &loop_cx);

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
        int top = getlabel();
        setlabel(top);

        if (cond && !stmt->always_taken())
            EmitTest(cond, false, loop_cx.break_to);

        EmitStmt(body);

        if (stmt->has_continue()) {
            setlabel(loop_cx.continue_to);
            EmitExpr(advance);
        }
        if (!body_always_exits)
            jumplabel(top);
    } else if (!stmt->never_taken()) {
        // continue:
        //   <cond>
        //   jf break
        //   <body>
        //   jmp continue
        // break:
        setlabel(loop_cx.continue_to);

        if (cond && !stmt->always_taken())
            EmitTest(cond, false, loop_cx.break_to);

        EmitStmt(body);

        if (!body_always_exits)
            jumplabel(loop_cx.continue_to);
    }
    setlabel(loop_cx.break_to);

    if (scope) {
        popheaplist(true);
        popstacklist(true);
    }
}

void
CodeGenerator::EmitSwitchStmt(SwitchStmt* stmt)
{
    EmitExpr(stmt->expr());

    auto exit_label = getlabel();
    auto table_label = getlabel();
    ffswitch(table_label);

    // Note: we use map for ordering so the case table is sorted.
    std::map<cell, int> case_labels;

    for (const auto& case_entry : stmt->cases()) {
        Stmt* stmt = case_entry.second;
        int label = getlabel();
        for (const auto& expr : case_entry.first) {
            const auto& v = expr->val();
            assert(v.ident == iCONSTEXPR);

            case_labels.emplace(v.constval, label);
        }

        setlabel(label);
        EmitStmt(stmt);
        if (!stmt->IsTerminal())
            jumplabel(exit_label);
    }

    int default_label = exit_label;
    if (stmt->default_case()) {
        default_label = getlabel();
        setlabel(default_label);
        EmitStmt(stmt->default_case());
        if (!stmt->default_case()->IsTerminal())
            jumplabel(exit_label);
    }

    setlabel(table_label);

    std::string deflabel = itoh(default_label);
    ffcase((int)case_labels.size(), deflabel.c_str(), TRUE);
    for (const auto& pair : case_labels) {
        deflabel = itoh(pair.second);
        ffcase(pair.first, deflabel.c_str(), FALSE);
    }

    setlabel(exit_label);
}

void
CodeGenerator::EmitFunctionInfo(FunctionInfo* info)
{
    ke::SaveAndSet<symbol*> set_func(&func_, info->sym());

    pc_max_func_memory = 0;
    pc_current_memory = 0;

    if (info->sym()->skipped)
        return;

    // :TODO: factor in usage
    cc_.functions().emplace(info->sym());

    if (!info->body())
        return;

    info->sym()->setAddr(code_idx);

    begcseg();
    startfunc(info->name()->chars());
    AddDebugLine(info->pos().line);
    setline(FALSE);
    pc_current_stack = 0;
    resetstacklist();
    resetheaplist();

    EmitStmt(info->body());

    assert(!has_stack_or_heap_scopes());

    // If return keyword is missing, we added it in the semantic pass.
    endfunc();

    info->sym()->codeaddr = code_idx;
    gDataQueue.Emit();

    pc_max_memory = std::max(pc_max_func_memory, pc_max_memory);
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
