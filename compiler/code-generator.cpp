// vim: set ts=8 sts=4 sw=4 tw=99 et:
//  Pawn compiler - Recursive descend expresion parser
//
//  Copyright (c) ITB CompuPhase, 1997-2005
//  Copyright (c) AlliedModders LLC, 2024
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

#include <map>
#include <optional>

#include <amtl/am-raii.h>

#include "array-helpers.h"
#include "assembler.h"
#include "code-generator.h"
#include "compile-context.h"
#include "compile-options.h"
#include "errors.h"
#include "expressions.h"
#include "sctracker.h"
#include "semantics-inl.h"
#include "symbols.h"
#include "value-inl.h"

namespace sp {
namespace cc {

#define __ asm_.

CodeGenerator::CodeGenerator(CompileContext& cc, ParseTree* tree)
  : cc_(cc),
    tree_(tree)
{
    sp::Atom* atom = cc_.atom("float");
    builtins_[atom] = &CodeGenerator::EmitFloatBuiltin;

    names_ = new SmxNameTable(".names");
    smx_data_ = new SmxDataSection(".data");
    natives_ = new SmxNativeSection(".natives");
    pubvars_ = new SmxPubvarSection(".pubvars");
    code_ = new SmxCodeSection(".code");
    publics_ = new SmxPublicSection(".publics");
    rtti_ = std::make_unique<RttiBuilder>(cc, names_);
}

bool CodeGenerator::Generate() {
    // First instruction is always halt.
    __ emit(OP_HALT, 0);

    EmitStmtList(tree_->stmts());
    if (!ComputeStackUsage())
        return false;

    // Finish any un-added debug symbols.
    while (!static_syms_.empty()) {
        auto pair = ke::PopBack(&static_syms_);
        AddDebugSymbols(&pair.second);
    }

    if (!errors_.ok())
        return false;

    AddDebugSymbols(&global_syms_);

    FinishSmx();
    return true;
}

void CodeGenerator::FinishSmx() {
    // Set up the data section. Note pre-SourceMod 1.7, the |memsize| was
    // computed as AMX::stp, which included the entire memory size needed to
    // store the file. Here (in 1.7+), we allocate what is actually needed
    // by the plugin.
    smx_data_->header().datasize = data_size();
    smx_data_->header().memsize = data_size() + DynamicMemorySize();
    smx_data_->header().data = sizeof(sp_file_data_t);
    smx_data_->setBlob(data_.dat(), data_.size());

    // Set up the code section.
    code_->header().codesize = asm_.size();
    code_->header().cellsize = sizeof(cell);
    code_->header().codeversion = SmxConsts::CODE_VERSION_TYPED_STACK;
    code_->header().flags = CODEFLAG_DEBUG;
    code_->header().main = 0;
    code_->header().code = sizeof(sp_file_code_t);
    code_->header().features = SmxConsts::kCodeFeatureDirectArrays |
                               SmxConsts::kCodeFeatureHeapScopes |
                               SmxConsts::kCodeFeatureNullFunctions |
                               SmxConsts::kCodeFeatureTypedOps;
    code_->setBlob(asm_.bytes(), asm_.size());

    smx_.add(code_);
    smx_.add(smx_data_);
    smx_.add(publics_);
    smx_.add(pubvars_);
    smx_.add(natives_);
    smx_.add(names_);
    rtti_->finish(smx_);
}

void CodeGenerator::AddDebugLine(const token_pos_t& pos) {
    auto line = cc_.sources()->GetLineAndCol(pos, nullptr);
    rtti_->AddDebugLine(asm_.position(), line);
}

void CodeGenerator::AddDebugSymbol(Decl* decl, uint32_t pc) {
    rtti_->AddDebugVar(fun_, decl, pc, asm_.position());
}

void CodeGenerator::AddDebugSymbols(tr::vector<DebugSymbol>* list) {
    while (!list->empty()) {
        auto entry = ke::PopBack(list);
        AddDebugSymbol(entry.first, entry.second);
    }
}

void
CodeGenerator::EmitStmtList(StmtList* list)
{
    for (const auto& stmt : list->stmts())
        EmitStmt(stmt);
}

void CodeGenerator::EmitStmt(Stmt* stmt) {
    std::list<std::pair<uint32_t, BuiltinType>> prev_used_temp_slots;

    if (fun_) {
        AddDebugLine(stmt->pos());
        EmitBreak();

        std::swap(prev_used_temp_slots, used_temp_slots_);
    }

    if (stmt->tree_has_heap_allocs())
        EnterHeapScope(stmt->flow_type());

    switch (stmt->kind()) {
        case StmtKind::ChangeScopeNode:
            EmitChangeScopeNode(stmt->to<ChangeScopeNode>());
            break;
        case StmtKind::ConstDecl:
        case StmtKind::VarDecl:
        case StmtKind::ArgDecl:
            EmitVarDecl(stmt->to<VarDeclBase>());
            break;
        case StmtKind::ExprStmt:
            // Emit even if no side effects.
            EmitExpr(stmt->to<ExprStmt>()->expr());
            break;
        case StmtKind::ExitStmt: {
            auto e = stmt->to<ExitStmt>();
            if (e->expr())
                EmitExpr(e->expr());
            else
                __ const_pri(0);
            __ emit(OP_HALT, xEXIT);
            break;
        }
        case StmtKind::BlockStmt: {
            auto s = stmt->to<BlockStmt>();

            {
                AutoEnterScope locals(this, &local_syms_);
                EmitStmtList(s);
            }
            break;
        }
        case StmtKind::AssertStmt: {
            auto s = stmt->to<AssertStmt>();
            Label flab1;
            EmitTest(s->expr(), true, &flab1);
            __ emit(OP_HALT, xASSERTION);
            __ bind(&flab1);
            break;
        }
        case StmtKind::IfStmt:
            EmitIfStmt(stmt->to<IfStmt>());
            break;
        case StmtKind::DeleteStmt:
            EmitDeleteStmt(stmt->to<DeleteStmt>());
            break;
        case StmtKind::DoWhileStmt:
            EmitDoWhileStmt(stmt->to<DoWhileStmt>());
            break;
        case StmtKind::BreakStmt:
            EmitLoopControl(tBREAK);
            break;
        case StmtKind::ContinueStmt:
            EmitLoopControl(tCONTINUE);
            break;
        case StmtKind::ForStmt:
            EmitForStmt(stmt->to<ForStmt>());
            break;
        case StmtKind::SwitchStmt:
            EmitSwitchStmt(stmt->to<SwitchStmt>());
            break;
        case StmtKind::FunctionDecl:
        case StmtKind::MemberFunctionDecl:
        case StmtKind::MethodmapMethodDecl:
            EmitFunctionDecl(stmt->to<FunctionDecl>());
            break;
        case StmtKind::EnumStructDecl:
            EmitEnumStructDecl(stmt->to<EnumStructDecl>());
            break;
        case StmtKind::MethodmapDecl:
            EmitMethodmapDecl(stmt->to<MethodmapDecl>());
            break;
        case StmtKind::ReturnStmt:
            EmitReturnStmt(stmt->to<ReturnStmt>());
            break;
        case StmtKind::TypedefDecl:
        case StmtKind::TypesetDecl:
        case StmtKind::EnumDecl:
        case StmtKind::EnumFieldDecl:
        case StmtKind::LayoutFieldDecl:
        case StmtKind::PstructDecl:
        case StmtKind::StaticAssertStmt:
        case StmtKind::PragmaUnusedStmt:
            break;
        case StmtKind::StmtList:
            EmitStmtList(stmt->to<StmtList>());
            break;

        default:
            assert(false);
    }

    if (stmt->tree_has_heap_allocs())
        LeaveHeapScope();

    if (fun_) {
        free_temp_slots_.splice(free_temp_slots_.end(), used_temp_slots_);
        std::swap(used_temp_slots_, prev_used_temp_slots);
    }
}

void CodeGenerator::EmitChangeScopeNode(ChangeScopeNode* node) {
    rtti_->AddDebugFile(asm_.position(), node->file()->chars());

    if (static_scopes_.count(node->scope())) {
        // We've already seen this scope before, which means we entered other
        // includes and then returned to this file.
        while (!static_syms_.empty()) {
            if (static_syms_.back().first == node->scope())
                break;
            auto pair = ke::PopBack(&static_syms_);

            // We left the include file, so assign all static variables a
            // debug entry.
            AddDebugSymbols(&pair.second);

            // Erase it so we know we left this scope.
            auto iter = static_scopes_.find(pair.first);
            assert(iter != static_scopes_.end());
            static_scopes_.erase(iter);
        }
        assert(!static_syms_.empty());
        assert(static_syms_.back().first == node->scope());
    } else {
        // This scope has not been seen before, so it's new.
        static_syms_.push_back({node->scope(), {}});
        static_scopes_.emplace(node->scope());
    }
}

void CodeGenerator::EmitVarDecl(VarDeclBase* decl) {
    if (decl->type()->isPstruct()) {
        EmitPstruct(decl);
    } else {
        if (!decl->as<ConstDecl>()) {
            if (decl->vclass() == sLOCAL)
                EmitLocalVar(decl);
            else
                EmitGlobalVar(decl);
        }
    }

    if (decl->is_public() || decl->is_used())
        EnqueueDebugSymbol(decl, asm_.position());

    if (decl->is_public()) {
        sp_file_pubvars_t& pubvar = pubvars_->add();
        pubvar.address = decl->addr();
        pubvar.name = names_->add(decl->name());
    }
}

void CodeGenerator::EmitGlobalVar(VarDeclBase* decl) {
    BinaryExpr* init = decl->init();

    __ bind_to(decl->label(), data_.dat_address());

    if (decl->type()->isArray() || decl->type()->isEnumStruct()) {
        ArrayData array;
        BuildCompoundInitializer(decl, &array, data_.dat_address());

        data_.Add(std::move(array.iv));
        data_.Add(std::move(array.data));
        data_.AddZeroes(array.zeroes);
    } else {
        if (init) {
            if (auto n64 = init->right()->as<Number64Expr>()) {
                Int64CellUnion u(*n64->ToInt64());
                data_.Add(u.cells[0]);
                data_.Add(u.cells[1]);
            } else {
                assert(init->right()->val().ident == iCONSTEXPR);
                data_.Add(init->right()->val().constval());
            }
        } else {
            cell_t cells = 1;
            if (auto es = decl->type()->asEnumStruct())
                cells = es->array_size();
            else if (decl->type()->isInt64())
                cells = 2;

            data_.AddZeroes(cells);
        }
    }
}

void CodeGenerator::EmitLocalVar(VarDeclBase* decl) {
    BinaryExpr* init = decl->init();

    bool is_struct = decl->type()->isEnumStruct();
    bool is_array = decl->type()->isArray();

    int num_cells;
    if (decl->type()->isBuiltin(BuiltinType::Int64))
        num_cells = 2;
    else
        num_cells = 1;

    int32_t slot = rtti_->AddLocalSlot(&locals_, decl->type());
    decl->BindAddress(slot);

    if (!is_array && !is_struct) {
        if (init) {
            const auto& val = init->right()->val();
            if (val.ident == iCONSTEXPR) {
                __ emit(OP_STOR_S_C, slot, val.constval());
            } else {
                EmitExpr(init->right());
                if (num_cells == 1)
                    __ emit(OP_STOR_S_PRI, slot);
                else if (num_cells == 2)
                    __ emit(OP_STOR_S_PRI_I64, slot);
                else
                    assert(false);
            }
        } else if (num_cells == 2) {
            __ emit(OP_ZERO_S_I64, slot);
        } else if (num_cells == 1) {
            // Note: we no longer honor "decl" for scalars.
            __ emit(OP_ZERO_S, slot);
        }
    } else {
        // Note that genarray() pushes the address onto the stack, so we don't
        // need to call modstk() here.
        TrackHeapAlloc(decl, MEMUSE_DYNAMIC, 0);

        auto init_rhs = decl->init_rhs();
        if (init_rhs && init_rhs->as<NewArrayExpr>()) {
            EmitExpr(init_rhs->as<NewArrayExpr>());
            __ emit(OP_POP_PRI);
            __ emit(OP_STOR_S_PRI, slot);
        } else if (!init_rhs || decl->type()->isArray() || is_struct) {
            ArrayData array;
            BuildCompoundInitializer(decl, &array, 0);

            cell iv_size = (cell)array.iv.size();
            cell data_size = (cell)array.data.size() + array.zeroes;
            cell total_size = iv_size + data_size;

            TrackHeapAlloc(decl, MEMUSE_STATIC, total_size);

            cell iv_addr = data_.dat_address();
            data_.Add(std::move(array.iv));
            data_.Add(std::move(array.data));

            if (array.zeroes < 16) {
                // For small numbers of extra zeroes, fold them into the data
                // section.
                data_.AddZeroes(array.zeroes);
                array.zeroes = 0;
            }

            cell non_filled = data_size - array.zeroes;

            // the decl keyword is deprecated, but we preserve its
            // optimization for older plugins so we don't introduce any
            // surprises. Note we zap the fill size *after* computing the
            // non-fill size, since we need to compute the copy size correctly.
            if (!decl->autozero() && array.zeroes)
                array.zeroes = 0;

            __ emit(OP_HEAP, total_size * sizeof(cell));
            __ emit(OP_INITARRAY_ALT, iv_addr, iv_size, non_filled, array.zeroes, 0);
            __ emit(OP_STOR_S_ALT, slot);
        } else if (StringExpr* ctor = init_rhs->as<StringExpr>()) {
            assert(false);
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
            __ copyarray(decl, cells * sizeof(cell));
        } else {
            assert(false);
        }
    }
}

void
CodeGenerator::EmitPstruct(VarDeclBase* decl)
{
    if (!decl->init())
        return;

    auto type = decl->type();
    auto ps = type->asPstruct();

    std::vector<cell> values;
    values.resize(ps->fields().size());

    auto init = decl->init_rhs()->as<StructExpr>();
    for (const auto& field : init->fields()) {
        auto arg = ps->FindField(field->name);
        if (auto expr = field->value->as<StringExpr>()) {
            values[arg->offset()] = data_.dat_address();
            data_.Add(expr->text()->chars(), expr->text()->length());
        } else if (auto expr = field->value->as<TaggedValueExpr>()) {
            values[arg->offset()] = expr->value();
        } else if (auto expr = field->value->as<SymbolExpr>()) {
            auto var = expr->decl()->as<VarDeclBase>();
            assert(var);
            values[arg->offset()] = var->addr();
        } else {
            assert(false);
        }
    }

    decl->BindAddress(data_.dat_address());

    for (const auto& value : values)
        data_.Add(value);
}

void
CodeGenerator::EmitExpr(Expr* expr)
{
    AutoErrorPos aep(expr->pos());

    if (expr->val().ident == iCONSTEXPR) {
        __ const_pri(expr->val().constval());
        return;
    }

    switch (expr->kind()) {
        case ExprKind::UnaryExpr:
            EmitUnary(expr->to<UnaryExpr>());
            break;
        case ExprKind::IncDecExpr:
            EmitIncDec(expr->to<IncDecExpr>());
            break;
        case ExprKind::BinaryExpr:
            EmitBinary(expr->to<BinaryExpr>());
            break;
        case ExprKind::LogicalExpr:
            EmitLogicalExpr(expr->to<LogicalExpr>());
            break;
        case ExprKind::ChainedCompareExpr:
            EmitChainedCompareExpr(expr->to<ChainedCompareExpr>());
            break;
        case ExprKind::TernaryExpr:
            EmitTernaryExpr(expr->to<TernaryExpr>());
            break;
        case ExprKind::CastExpr:
            EmitCastExpr(expr->to<CastExpr>());
            break;
        case ExprKind::SymbolExpr:
            EmitSymbolExpr(expr->to<SymbolExpr>());
            break;
        case ExprKind::RvalueExpr: {
            auto e = expr->to<RvalueExpr>();
            EmitExpr(e->expr());
            EmitRvalue(e->expr()->val());
            break;
        }
        case ExprKind::CommaExpr: {
            auto ce = expr->to<CommaExpr>();
            for (const auto& expr : ce->exprs())
                EmitExpr(expr);
            break;
        }
        case ExprKind::ThisExpr: {
            auto e = expr->to<ThisExpr>();
            if (e->decl()->type()->isEnumStruct())
                __ address(e->decl(), sPRI);
            break;
        }
        case ExprKind::StringExpr: {
            auto se = expr->to<StringExpr>();
            auto addr = data_.dat_address();
            data_.Add(se->text()->chars(), se->text()->length());
            __ const_pri(addr);
            break;
        }
        case ExprKind::ArrayExpr: {
            auto e = expr->to<ArrayExpr>();
            auto addr = data_.dat_address();
            for (const auto& expr : e->exprs())
                data_.Add(expr->val().constval());
            __ const_pri(addr);
            break;
        }
        case ExprKind::IndexExpr:
            EmitIndexExpr(expr->to<IndexExpr>());
            break;
        case ExprKind::FieldAccessExpr:
            EmitFieldAccessExpr(expr->to<FieldAccessExpr>());
            break;
        case ExprKind::CallExpr:
            EmitCallExpr(expr->to<CallExpr>());
            break;
        case ExprKind::DefaultArgExpr:
            EmitDefaultArgExpr(expr->to<DefaultArgExpr>());
            break;
        case ExprKind::NewArrayExpr:
            EmitNewArrayExpr(expr->to<NewArrayExpr>());
            break;
        case ExprKind::NamedArgExpr:
            EmitExpr(expr->to<NamedArgExpr>()->expr);
            break;
        case ExprKind::Number64Expr:
            EmitNumber64Expr(expr->to<Number64Expr>());
            break;
        case ExprKind::SimpleCastExpr:
            EmitSimpleCastExpr(expr->to<SimpleCastExpr>());
            break;

        default:
            assert(false);
    }
}

void
CodeGenerator::EmitTest(Expr* expr, bool jump_on_true, Label* target)
{
    switch (expr->kind()) {
        case ExprKind::LogicalExpr:
            EmitLogicalExprTest(expr->to<LogicalExpr>(), jump_on_true, target);
            return;
        case ExprKind::UnaryExpr:
            if (EmitUnaryExprTest(expr->to<UnaryExpr>(), jump_on_true, target))
                return;
            break;
        case ExprKind::BinaryExpr:
            if (EmitBinaryExprTest(expr->to<BinaryExpr>(), jump_on_true, target))
                return;
            break;
        case ExprKind::CommaExpr: {
            auto ce = expr->to<CommaExpr>();
            for (size_t i = 0; i < ce->exprs().size() - 1; i++)
                EmitExpr(ce->exprs().at(i));

            EmitTest(ce->exprs().back(), jump_on_true, target);
            return;
        }
    }

    EmitExpr(expr);

    assert(!expr->val().type()->isInt64());

    if (jump_on_true)
        __ emit(OP_JNZ, target);
    else
        __ emit(OP_JZER, target);
}

void
CodeGenerator::EmitUnary(UnaryExpr* expr)
{
    auto inner = expr->expr();
    EmitExpr(inner);

    switch (expr->token()) {
        case '~':
            if (inner->val().type()->isInt64()) {
                auto slot = AcquireTempSlot(BuiltinType::Int64);
                __ emit(OP_INVERT_I64, slot);
            } else {
                __ emit(OP_INVERT);
            }
            break;
        case '!':
            if (inner->val().type()->isInt64())
                __ emit(OP_TEST_I64);
            else if (inner->val().type()->isFloat())
                __ emit(OP_TEST_F32);
            __ emit(OP_NOT);
            break;
        case '-':
            if (inner->val().type()->isInt64()) {
                auto slot = AcquireTempSlot(BuiltinType::Int64);
                __ emit(OP_NEG_I64, slot);
            } else if (inner->val().type()->isFloat()) {
                __ emit(OP_NEG_F32);
            } else {
                __ emit(OP_NEG);
            }
            break;
        default:
            assert(false);
    }
}

bool
CodeGenerator::EmitUnaryExprTest(UnaryExpr* expr, bool jump_on_true, Label* target)
{
    if (expr->token() == '!') {
        auto inner = expr->expr();
        if (!inner->val().type()->isInt64()) {
            EmitTest(expr->expr(), !jump_on_true, target);
            return true;
        }
    }
    return false;
}

void
CodeGenerator::EmitIncDec(IncDecExpr* expr)
{
    EmitExpr(expr->expr());

    const auto& val = expr->expr()->val();

    Type* type = val.type();
    if (type->isReference())
        type = type->inner();

    cell_t inc_int64_slot = -1;
    if (type->isInt64()) {
        Int64CellUnion u(expr->token() == tINC ? 1 : -1);
        inc_int64_slot = AcquireTempSlot(BuiltinType::Int64);
        __ emit(OP_STOR_S_C_I64, inc_int64_slot, u.cells[0], u.cells[1]);
    }

    // Save base address if needed.
    if (!val.canRematerialize())
        __ emit(OP_PUSH_PRI);

    EmitRvalue(val);

    bool want_pre_value = !(expr->prefix() || expr->discard());
    if (want_pre_value) {
        if (type->isInt64()) {
            cell_t pre_slot = AcquireTempSlot(BuiltinType::Int64);
            __ emit(OP_ADDR_ALT, pre_slot);
            __ emit(OP_MOVE_I64);
            __ emit(OP_PUSH_ALT);
        } else {
            __ emit(OP_PUSH_PRI);
        }
    }

    if (type->isInt64()) {
        // alt is inc_slot, pri is original address.
        // After this, pri = inc_slot.
        __ emit(OP_ADDR_ALT, inc_int64_slot);
        __ emit(OP_ADD_I64, inc_int64_slot);
    } else if (type->isFloat()) {
        float val = (expr->token() == tINC ? 1.0f : -1.0f);
        __ emit(OP_CONST_ALT, sp_ftoc(val));
        __ emit(OP_ADD_F32);
    } else {
        __ emit(expr->token() == tINC ? OP_INC_PRI : OP_DEC_PRI);
    }

    if (!val.canRematerialize()) {
        // If want_pre_value, then ALT will have the "pre" value. Otherwise,
        // it is the original lvalue address.
        __ emit(OP_POP_ALT);
    }

    if (want_pre_value) {
        // If we pushed the original lvalue address onto the stack, it's still
        // there, and ALT has the "pre" value. In that case we need to swap
        // them. Then ALT will have the address for the store operation, and
        // we can later pop the return value into PRI.
        if (!val.canRematerialize())
            __ emit(OP_SWAP_ALT);
        EmitStore(val, false /* save_pri */);
        __ emit(OP_POP_PRI);
    } else {
        EmitStore(val, !expr->discard() /* save_pri */);
    }
}

void
CodeGenerator::EmitBinary(BinaryExpr* expr)
{
    auto token = expr->token();
    auto left = expr->left();
    auto right = expr->right();
    auto oper = NormalizeBinaryToken(expr->token());

    // We emit constexprs in the |oper_| handler below.
    const auto& left_val = left->val();
    if (IsAssignOp(token) || left_val.ident != iCONSTEXPR)
        EmitExpr(left);

    bool saved_lhs = false;
    if (IsAssignOp(token)) {
        if (left_val.ident == iARRAYCELL || left_val.ident == iARRAYCHAR ||
            left_val.type()->isArray())
        {
            if (oper) {
                __ emit(OP_PUSH_PRI);
                EmitRvalue(left_val);
                saved_lhs = true;
            }
        } else if (left_val.ident == iACCESSOR) {
            __ emit(OP_PUSH_PRI);
            if (oper)
                EmitRvalue(left_val);
            saved_lhs = true;
        } else {
            assert(left->lvalue());
            if (oper)
                EmitRvalue(left_val);
        }

        if (expr->array_copy_length()) {
            assert(!oper);

            __ emit(OP_PUSH_PRI);
            EmitExpr(right);
            __ emit(OP_POP_ALT);
            __ emit(OP_MOVS, expr->array_copy_length() * sizeof(cell));
            return;
        }
    }

    assert(!expr->array_copy_length());
    assert(!left_val.type()->isArray());

    EmitBinaryInner(expr, oper, left, right);

    if (IsAssignOp(token)) {
        if (saved_lhs)
            __ emit(OP_POP_ALT);

        EmitStore(left_val);
    }
}

void CodeGenerator::EmitBinaryInner(Expr* expr, int oper_tok, Expr* left, Expr* right) {
    const auto& left_val = left->val();
    const auto& right_val = right->val();

    // left goes into ALT, right goes into PRI, though we can swap them for
    // commutative operations.
    if (left_val.ident == iCONSTEXPR) {
        if (right_val.ident == iCONSTEXPR)
            __ const_pri(right_val.constval());
        else
            EmitExpr(right);
        __ const_alt(left_val.constval());
    } else {
        // If performing a binary operation, we need to make sure the LHS winds
        // up in ALT. If performing a store, we only need to preserve LHS to
        // ALT if it can't be re-evaluated.
        bool must_save_lhs = oper_tok || !left_val.canRematerialize();
        if (right_val.ident == iCONSTEXPR) {
            if (commutative(oper_tok)) {
                __ const_alt(right_val.constval());
            } else {
                if (must_save_lhs)
                    __ emit(OP_PUSH_PRI);
                __ const_pri(right_val.constval());
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

    Type* effective = left->val().type();
    if (effective->isReference())
        effective = effective->inner();

    BuiltinType type = BuiltinType::Int;
    if (effective->isInt64())
        type = BuiltinType::Int64;
    else if (effective->isFloat())
        type = BuiltinType::Float;

    if (oper_tok)
        EmitBinaryOp(expr, type, oper_tok);
}

OPCODE GetFloatBinaryOp(int oper_tok) {
    switch (oper_tok) {
        case '*': return OP_MUL_F32;
        case '/': return OP_DIV_ALT_F32;
        case '%': return OP_MOD_ALT_F32;
        case '+': return OP_ADD_F32;
        case '-': return OP_SUB_ALT_F32;
        case tlEQ: return OP_EQ_F32;
        case tlNE: return OP_NEQ_F32;
        case '>': return OP_GRTR_F32;
        case tlGE: return OP_GEQ_F32;
        case '<': return OP_LESS_F32;
        case tlLE: return OP_LEQ_F32;
        default:
            assert(false);
            return OP_NONE;
    }
}

OPCODE GetInt32BinaryOp(int oper_tok) {
    switch (oper_tok) {
        case '*': return OP_SMUL;
        case '/': return OP_SDIV_ALT_I32;
        case '%': return OP_SMOD_ALT_I32;
        case '+': return OP_ADD;
        case '-': return OP_SUB_ALT;
        case tSHL: return OP_SHL;
        case tSHR: return OP_SSHR;
        case tSHRU: return OP_SHR;
        case '&': return OP_AND;
        case '^': return OP_XOR;
        case '|': return OP_OR;
        case tlEQ: return OP_EQ;
        case tlNE: return OP_NEQ;
        case '>': return OP_SGRTR;
        case tlGE: return OP_SGEQ;
        case '<': return OP_SLESS;
        case tlLE: return OP_SLEQ;
        default:
            assert(false);
            return OP_NONE;
    }
}

OPCODE GetInt64BinaryOp(int oper_tok) {
    switch (oper_tok) {
        case '*': return OP_SMUL_I64;
        case '/': return OP_SDIV_ALT_I64;
        case '%': return OP_SMOD_ALT_I64;
        case '+': return OP_ADD_I64;
        case '-': return OP_SUB_ALT_I64;
        case tSHL: return OP_SHL_I64;
        case tSHR: return OP_SSHR_I64;
        case tSHRU: return OP_SHR_I64;
        case '&': return OP_AND_I64;
        case '^': return OP_XOR_I64;
        case '|': return OP_OR_I64;
        case tlEQ: return OP_EQ_I64;
        case tlNE: return OP_NEQ_I64;
        case '>': return OP_SGRTR_I64;
        case tlGE: return OP_SGEQ_I64;
        case '<': return OP_SLESS_I64;
        case tlLE: return OP_SLEQ_I64;
        default:
            assert(false);
            return OP_NONE;
    }
}

static inline bool IsCompareOp(int oper_tok) {
    switch (oper_tok) {
        case tlEQ:
        case tlNE:
        case '>':
        case tlGE:
        case '<':
        case tlLE:
            return true;
        default:
            return false;
    }
}

void CodeGenerator::EmitBinaryOp(Expr* expr, BuiltinType type, int oper_tok) {
    switch (oper_tok) {
        case tlLE:
        case tlGE:
        case '<':
        case '>':
        case tSHL:
        case tSHR:
        case tSHRU:
            __ emit(OP_XCHG);
            break;
    }

    if (type == BuiltinType::Int64) {
        OPCODE op64 = GetInt64BinaryOp(oper_tok);
        if (IsCompareOp(oper_tok)) {
            __ emit(op64);
        } else {
            auto pri_slot = AcquireTempSlot(BuiltinType::Int64);
            __ emit(op64, pri_slot);
        }
    } else if (type == BuiltinType::Float) {
        __ emit(GetFloatBinaryOp(oper_tok));
    } else {
        __ emit(GetInt32BinaryOp(oper_tok));
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
        case tlEQ:
            return OP_JEQ;
        case tlNE:
            return OP_JNEQ;
        default:
            assert(false);
            return OP_HALT;
    }
}

bool CodeGenerator::EmitBinaryExprTest(BinaryExpr* root, bool jump_on_true, Label* target) {
    if (!IsCompare(root->token()))
        return false;

    Expr* left = root->left();
    if (left->val().type()->isInt64() || left->val().type()->isFloat())
        return false;

    Expr* right = root->right();

    EmitExpr(left);
    __ emit(OP_PUSH_PRI);
    EmitExpr(right);
    __ emit(OP_POP_ALT);
    __ emit(OP_XCHG);

    int token = root->token();
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
            case tlEQ:
                token = tlNE;
                break;
            case tlNE:
                token = tlEQ;
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

        int oper_tok = NormalizeBinaryToken(op.token);
        EmitBinaryInner(root, oper_tok, left, op.expr);

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

    __ emit(OP_JZER, &flab1);
    EmitExpr(expr->second());
    __ emit(OP_JUMP, &flab2);
    __ bind(&flab1);
    EmitExpr(expr->third());
    __ bind(&flab2);
}

void
CodeGenerator::EmitSymbolExpr(SymbolExpr* expr)
{
    Decl* sym = expr->decl();
    if (auto fun = sym->as<FunctionDecl>()) {
        assert(fun == fun->canonical());

        assert(!fun->is_native());
        assert(fun->is_live());
        __ emit(OP_CONST_PRI, &fun->cg()->funcid);
    } else if (auto var = sym->as<VarDeclBase>()) {
        if (sym->type()->isArray() || sym->type()->isEnumStruct())
            __ address(var, sPRI);
    } else {
        assert(false);
    }
}

void
CodeGenerator::EmitIndexExpr(IndexExpr* expr)
{
    EmitExpr(expr->base());

    auto& base_val = expr->base()->val();

    cell_t rank_size = sizeof(cell_t);

    auto array_type = base_val.type()->as<ArrayType>();
    if (!array_type->inner()->isArray()) {
        if (array_type->inner()->isChar())
            rank_size = 1;
        else if (array_type->inner()->isInt64())
            rank_size = sizeof(cell_t) * 2;
        else if (auto es = array_type->inner()->asEnumStruct())
            rank_size = es->array_size() * sizeof(cell_t);
    }

    assert(rank_size == 1 || (rank_size % sizeof(cell_t) == 0));

    const auto& idxval = expr->index()->val();
    if (idxval.ident == iCONSTEXPR) {
        if (idxval.constval() != 0)
            __ emit(OP_ADD_C, idxval.constval() * rank_size);
    } else {
        __ emit(OP_PUSH_PRI);
        EmitExpr(expr->index());

        if (array_type->size()) {
            __ emit(OP_BOUNDS, array_type->size() - 1); /* run time check for array bounds */
        } else {
            // vm uses unsigned compare, this protects against negative indices.
            __ emit(OP_BOUNDS, INT_MAX); 
        }

        if (rank_size == 1) {
            __ emit(OP_POP_ALT);
            __ emit(OP_ADD);
        } else {
            __ emit(OP_POP_ALT);
            if (rank_size != sizeof(cell_t))
                __ emit(OP_SMUL_C, rank_size / sizeof(cell_t));
            __ emit(OP_IDXADDR);
        }
    }

    // The indexed item is another array (multi-dimensional arrays).
    if (array_type->inner()->isArray()) {
        assert(expr->val().type()->isArray());
        __ emit(OP_LOAD_I);
    }
}

void
CodeGenerator::EmitFieldAccessExpr(FieldAccessExpr* expr)
{
    assert(expr->token() == '.');

    // Note that we do not load an iACCESSOR here, we only make sure the base
    // is computed. Emit() never performs loads on l-values, that ability is
    // reserved for RvalueExpr().
    EmitExpr(expr->base());

    // Only enum struct accesses have a resolved decl.
    if (!expr->resolved())
        return;

    if (LayoutFieldDecl* field = expr->resolved()->as<LayoutFieldDecl>()) {
        if (field->offset()) {
            __ const_alt(field->offset() << 2);
            __ emit(OP_ADD);
        }
    }
}

void CodeGenerator::EmitCallExpr(CallExpr* call) {
    if (call->fun()->is_builtin()) {
        auto iter = builtins_.find(call->fun()->name());
        assert(iter != builtins_.end());

        (this->*(iter->second))(call);
        return;
    }

    // Calculate the hidden parameter if needed. If we need to heap allocate,
    // we store the address in a local slot, so we can easily read it back out
    // after the function returns. For simple stack allocations we just use a
    // local variable.
    std::optional<cell_t> hidden_arg;
    std::optional<cell_t> hidden_slot;
    cell_t nargs = (cell_t)call->args().size();
    if (call->fun()->needs_hidden_arg()) {
        auto return_type = call->fun()->return_type();
        if (call->fun()->is_native() && return_type->isArray()) {
            EmitNativeCallHiddenArg(call);

            hidden_arg = {AcquireTempSlot(BuiltinType::Int)};
            __ emit(OP_STOR_S_ALT, *hidden_arg);
        } else if (return_type->isArray() || return_type->isEnumStruct()) {
            cell retsize = call->fun()->return_type()->CellStorageSize();
            assert(retsize);

            hidden_arg = {AcquireTempSlot(BuiltinType::Int)};
            __ emit(OP_HEAP, retsize * sizeof(cell));
            __ emit(OP_STOR_S_ALT, *hidden_arg);
            TrackTempHeapAlloc(call, 1);
        } else {
            hidden_slot = {AcquireTempSlot(BuiltinType::Int64)};
        }
        nargs++;
    }

    const auto& argv = call->args();
    const auto& arginfov = call->fun()->args();
    for (size_t i = argv.size() - 1; i < argv.size(); i--) {
        const auto& expr = argv[i];

        EmitExpr(expr);

        if (expr->as<DefaultArgExpr>()) {
            __ emit(OP_PUSH_PRI);
            continue;
        }

        const auto& val = expr->val();
        bool lvalue = expr->lvalue();

        ArgDecl* arg;
        if (i < arginfov.size()) {
            arg = arginfov[i];
        } else {
            arg = arginfov.back();
            assert(arg->type_info().is_varargs);
        }

        if (arg->type_info().is_varargs) {
            bool needs_temp = false;
            if (val.type()->isArray()) {
                if (lvalue)
                    __ address(val.sym, sPRI);
            } else if (val.ident == iVARIABLE) {
                assert(val.sym);
                assert(lvalue);
                /* treat a "const" variable passed to a function with a non-const
                 * "variable argument list" as a constant here */
                if (val.sym->is_const() && !arg->type_info().is_const) {
                    EmitRvalue(val);
                    needs_temp = true;
                } else if (lvalue) {
                    __ address(val.sym, sPRI);
                } else {
                    needs_temp = true;
                }
            } else if (val.ident == iCONSTEXPR || val.ident == iEXPRESSION) {
                needs_temp = true;
            }
            if (needs_temp) {
                if (val.type()->isInt64()) {
                    auto slot = AcquireTempSlot(BuiltinType::Int64);
                    __ emit(OP_ADDR_ALT, slot);
                    __ emit(OP_MOVE_I64);
                    __ emit(OP_MOVE_PRI);
                } else {
                    auto slot = AcquireTempSlot(BuiltinType::Int);
                    __ emit(OP_STOR_S_PRI, slot);
                    __ emit(OP_ADDR_PRI, slot);
                }
            }
        } else {
            if (arg->type_info().type->isReference()) {
                if (val.ident == iVARIABLE) {
                    assert(val.sym);
                    __ address(val.sym, sPRI);
                }
            } else {
                // Emit a copy since int64 is passed by value. Unfortunately
                // we can't tell if this is already a copy :(
                // Maybe we could use iEXPRESSION as an indicator?
                if (val.type()->isInt64()) {
                    int32_t slot = AcquireTempSlot(BuiltinType::Int64);
                    __ emit(OP_ADDR_ALT, slot);
                    __ emit(OP_MOVE_I64);
                    __ emit(OP_MOVE_PRI);
                }
            }
        }

        __ emit(OP_PUSH_PRI);
    }

    if (hidden_arg)
        __ emit(OP_PUSH_S, *hidden_arg);
    else if (hidden_slot)
        __ emit(OP_PUSH_ADR, *hidden_slot);

    EmitCall(call->fun(), nargs);

    if (hidden_arg)
        __ emit(OP_LOAD_S_PRI, *hidden_arg);
    else if (hidden_slot)
        __ emit(OP_ADDR_PRI, *hidden_slot);
}

void CodeGenerator::EmitNativeCallHiddenArg(CallExpr* call) {
    TrackTempHeapAlloc(call, 1);

    auto fun = call->fun();

    ArrayData array;
    BuildCompoundInitializer(QualType(fun->return_type()), nullptr, &array);

    cell retsize = call->fun()->return_type()->CellStorageSize();
    assert(retsize);

    __ emit(OP_HEAP, retsize * sizeof(cell));

    auto info = fun->return_array();
    if (array.iv.empty()) {
        __ emit(OP_CONST_PRI, 0);
        __ emit(OP_FILL, retsize);
    } else {
        if (!info->iv_size) {
            // No initializer, so we should have no data.
            assert(array.data.empty());
            assert(array.zeroes);

            info->iv_size = (cell_t)array.iv.size();
            info->dat_addr = data_.dat_address();
            info->zeroes = array.zeroes;
            data_.Add(std::move(array.iv));
        }

        cell dat_addr = info->dat_addr;
        cell iv_size = info->iv_size;
        assert(iv_size);
        assert(info->zeroes);

        __ emit(OP_INITARRAY_ALT, dat_addr, iv_size, 0, info->zeroes, 0);
    }
}

void
CodeGenerator::EmitDefaultArgExpr(DefaultArgExpr* expr)
{
    const auto& arg = expr->arg();
    if (arg->type()->isReference()) {
        __ const_pri(arg->default_value()->val.get());
        __ setheap_pri();
        TrackTempHeapAlloc(expr, 1);
    } else if (arg->type()->isArray()) {
        EmitDefaultArray(expr, arg);
    } else {
        if (arg->type()->isEnumStruct())
            EmitDefaultArray(expr, arg);
        else
            __ const_pri(arg->default_value()->val.get());
    }
}

void CodeGenerator::EmitNewArrayExpr(NewArrayExpr* expr) {
    const auto& type = expr->type();
    auto innermost = type;
    while (innermost->isArray())
        innermost = innermost->to<ArrayType>()->inner();

    int numdim = 0;
    auto& exprs = expr->exprs();
    for (size_t i = 0; i < exprs.size(); i++) {
        EmitExpr(exprs[i]);

        if (i == exprs.size() - 1) {
            if (innermost->isChar()) {
                __ emit(OP_STRADJUST_PRI);
            } else if (auto es = innermost->asEnumStruct(); es && es->array_size() > 1) {
                cell_t max_size = kMaxCells / es->array_size();
                __ emit(OP_BOUNDS, max_size);
                __ emit(OP_SMUL_C, es->array_size());
            }
        }

        __ emit(OP_PUSH_PRI);
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
        if (stmt->on_true()->flow_type() == Flow_None)
            __ emit(OP_JUMP, &flab2);
        __ bind(&flab1);
        EmitStmt(stmt->on_false());
        if (flab2.used())
            __ bind(&flab2);
    } else {
        __ bind(&flab1);
    }
}

void CodeGenerator::EmitReturnArrayStmt(ReturnStmt* stmt) {
    ArrayData array;
    BuildCompoundInitializer(QualType(stmt->expr()->val().type()), nullptr, &array);

    auto info = fun_->return_array();
    if (array.iv.empty()) {
        // A much simpler copy can be emitted.
        __ load_hidden_arg(fun_);

        cell size = fun_->return_type()->CellStorageSize();
        __ emit(OP_MOVS, size * sizeof(cell));
        return;
    }

    if (!info->iv_size) {
        // No initializer, so we should have no data.
        assert(array.data.empty());
        assert(array.zeroes);

        info->iv_size = (cell_t)array.iv.size();
        info->dat_addr = data_.dat_address();
        info->zeroes = array.zeroes;
        data_.Add(std::move(array.iv));
    }

    cell dat_addr = info->dat_addr;
    cell iv_size = info->iv_size;
    assert(iv_size);
    assert(info->zeroes);

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
    __ load_hidden_arg(fun_);
    __ emit(OP_INITARRAY_ALT, dat_addr, iv_size, 0, 0, 0);
    __ emit(OP_MOVE_PRI);
    __ emit(OP_ADD_C, iv_size * sizeof(cell));
    __ emit(OP_MOVE_ALT);
    __ emit(OP_POP_PRI);
    __ emit(OP_ADD_C, iv_size * sizeof(cell));
    __ emit(OP_MOVS, info->zeroes * sizeof(cell));
}

void
CodeGenerator::EmitReturnStmt(ReturnStmt* stmt)
{
    if (stmt->expr()) {
        EmitExpr(stmt->expr());

        const auto& v = stmt->expr()->val();
        if (v.type()->isArray() || v.type()->isEnumStruct()) {
            EmitReturnArrayStmt(stmt);
        } else if (v.type()->isInt64()) {
            // Must copy to the hidden arg.
            __ load_hidden_arg(fun_);
            __ emit(OP_MOVE_I64);
        }
    } else {
        /* this return statement contains no expression */
        assert(!fun_->return_type()->isInt64());
        __ const_pri(0);
    }

    __ emit(OP_RETN);
}

void
CodeGenerator::EmitDeleteStmt(DeleteStmt* stmt)
{
    Expr* expr = stmt->expr();
    const auto& v = expr->val();

    // Only zap non-const lvalues.
    bool zap = expr->lvalue();
    if (zap && v.sym && v.sym->is_const())
        zap = false;

    EmitExpr(expr);

    bool popaddr = false;
    MethodmapPropertyDecl* accessor = nullptr;
    if (expr->lvalue()) {
        if (zap) {
            switch (v.ident) {
                case iACCESSOR:
                    // EmitRvalue() removes iACCESSOR so we store it locally.
                    accessor = v.accessor();
                    if (!accessor->setter()) {
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

        EmitRvalue(v);
    }

    // push.pri
    // push.c 1
    // sysreq.c N 1
    // stack 8
    __ emit(OP_PUSH_PRI);
    EmitCall(stmt->map()->dtor(), 1);

    if (zap) {
        if (popaddr)
            __ emit(OP_POP_ALT);

        // Store 0 back.
        __ const_pri(0);
        if (accessor)
            InvokeSetter(accessor, FALSE);
        else
            EmitStore(v);
    }
}

void CodeGenerator::EmitRvalue(const value& lval) {
    switch (lval.ident) {
        case iARRAYCELL:
            if (!lval.type()->isComposite())
                __ emit(OP_LOAD_I);
            break;
        case iARRAYCHAR:
            __ emit(OP_LODB_I, 1);
            break;
        case iACCESSOR:
            InvokeGetter(lval.accessor());
            break;
        case iVARIABLE: {
            if (lval.type()->isReference()) {
                auto var = lval.sym->as<VarDeclBase>();
                assert(var->vclass() == sLOCAL || var->vclass() == sARGUMENT);
                // int64 is internally passed by address.
                if (lval.type()->inner()->isInt64())
                    __ emit(OP_LOAD_S_PRI, var->addr());
                else
                    __ emit(OP_LREF_S_PRI, var->addr());
                break;
            }
            [[fallthrough]];
        }
        default: {
            auto var = lval.sym->as<VarDeclBase>();
            if (var->type()->isInt64())
                __ address(var, sPRI);
            else if (var->vclass() == sLOCAL || var->vclass() == sARGUMENT)
                __ emit(OP_LOAD_S_PRI, var->addr());
            else if (!var->type()->isComposite())
                __ emit(OP_LOAD_PRI, var->label());
            break;
        }
    }
}

void
CodeGenerator::EmitStore(const value& lval, bool save_pri)
{
    switch (lval.ident) {
        case iARRAYCELL:
            if (lval.type()->isInt64())
                __ emit(OP_MOVE_I64);
            else
                __ emit(OP_STOR_I);
            break;
        case iARRAYCHAR:
            __ emit(OP_STRB_I, 1);
            break;
        case iACCESSOR:
            InvokeSetter(lval.accessor(), save_pri);
            break;
        case iVARIABLE: {
            if (lval.type()->isReference()) {
                auto var = lval.sym->as<VarDeclBase>();
                assert(var->vclass() == sLOCAL || var->vclass() == sARGUMENT);

                if (lval.type()->inner()->isInt64()) {
                    __ emit(OP_LOAD_S_ALT, var->addr());
                    __ emit(OP_MOVE_I64);
                } else {
                    __ emit(OP_SREF_S_PRI, var->addr());
                }
                break;
            }
            [[fallthrough]];
        }
        default: {
            auto var = lval.sym->as<VarDeclBase>();
            if (var->vclass() == sLOCAL || var->vclass() == sARGUMENT) {
                if (var->type()->isInt64()) {
                    __ emit(OP_ADDR_ALT, var->addr());
                    __ emit(OP_MOVE_I64);
                } else {
                    __ emit(OP_STOR_S_PRI, var->addr());
                }
            } else {
                if (var->type()->isInt64()) {
                    __ emit(OP_CONST_ALT, var->addr());
                    __ emit(OP_MOVE_I64);
                } else {
                    __ emit(OP_STOR_PRI, var->addr());
                }
            }
            break;
        }
    }
}

void CodeGenerator::InvokeGetter(MethodmapPropertyDecl* prop) {
    assert(prop->getter());

    // :TODO: figure out how to factor this code with EmitCallExpr.
    std::optional<cell_t> hidden_slot;
    if (prop->getter()->return_type()->isInt64())
        hidden_slot = {AcquireTempSlot(BuiltinType::Int64)};

    // |this|
    __ emit(OP_PUSH_PRI);

    cell_t nargs = 1;
    if (hidden_slot) {
        __ emit(OP_PUSH_ADR, *hidden_slot);
        nargs++;
    }

    EmitCall(prop->getter(), nargs);

    if (hidden_slot)
        __ emit(OP_ADDR_PRI, *hidden_slot);
}

void CodeGenerator::InvokeSetter(MethodmapPropertyDecl* prop, bool save_pri) {
    assert(prop->setter());

    if (save_pri)
      __ emit(OP_PUSH_PRI);
    __ emit(OP_PUSH_PRI);
    __ emit(OP_PUSH_ALT);
    EmitCall(prop->setter(), 2);
    if (save_pri)
      __ emit(OP_POP_PRI);
}

void
CodeGenerator::EmitDoWhileStmt(DoWhileStmt* stmt)
{
    int token = stmt->token();
    assert(token == tDO || token == tWHILE);

    LoopContext loop_cx;
    loop_cx.heap_scope_id = heap_scope_id();
    ke::SaveAndSet<LoopContext*> push_context(&loop_, &loop_cx);

    auto body = stmt->body();
    auto cond = stmt->cond();
    if (token == tDO) {
        Label start;
        __ bind(&start);

        EmitStmt(body);

        if (!IsTerminalFlow(body->flow_type()) || loop_cx.continue_to.used()) {
            __ bind(&loop_cx.continue_to);
            if (cond->tree_has_heap_allocs()) {
                // Need to create a temporary heap scope here.
                Label on_true, join;
                EnterHeapScope(Flow_None);
                EmitTest(cond, true, &on_true);
                __ emit(OP_PUSH_C, 0);
                __ emit(OP_JUMP, &join);
                __ bind(&on_true);
                __ emit(OP_PUSH_C, 1);
                __ bind(&join);
                LeaveHeapScope();
                __ emit(OP_POP_PRI);
                __ emit(OP_JNZ, &start);
            } else {
                EmitTest(cond, true, &start);
            }
        }
    } else {
        __ bind(&loop_cx.continue_to);

        if (cond->tree_has_heap_allocs()) {
            // Need to create a temporary heap scope here.
            Label on_true, join;
            EnterHeapScope(Flow_None);
            EmitTest(cond, true, &on_true);
            __ emit(OP_PUSH_C, 0);
            __ emit(OP_JUMP, &join);
            __ bind(&on_true);
            __ emit(OP_PUSH_C, 1);
            __ bind(&join);
            LeaveHeapScope();
            __ emit(OP_POP_PRI);
            __ emit(OP_JZER, &loop_cx.break_to);
        } else {
            EmitTest(cond, false, &loop_cx.break_to);
        }
        EmitStmt(body);
        if (body->flow_type() == Flow_None)
            __ emit(OP_JUMP, &loop_cx.continue_to);
    }

    __ bind(&loop_cx.break_to);
}

void
CodeGenerator::EmitLoopControl(int token)
{
    assert(loop_);
    assert(token == tBREAK || token == tCONTINUE);

    for (auto iter = heap_scopes_.rbegin(); iter != heap_scopes_.rend(); iter++) {
        if (iter->scope_id == loop_->heap_scope_id)
            break;
        if (iter->needs_restore)
            __ emit(OP_HEAP_RESTORE);
    }

    if (token == tBREAK)
        __ emit(OP_JUMP, &loop_->break_to);
    else
        __ emit(OP_JUMP, &loop_->continue_to);
}

void CodeGenerator::EmitForStmt(ForStmt* stmt) {
    ke::Maybe<AutoEnterScope> debug_scope;

    auto scope = stmt->scope();
    if (scope)
        debug_scope.init(this, &local_syms_);

    auto init = stmt->init();
    if (init)
        EmitStmt(init);

    LoopContext loop_cx;
    loop_cx.heap_scope_id = heap_scope_id();
    ke::SaveAndSet<LoopContext*> push_context(&loop_, &loop_cx);

    auto body = stmt->body();
    bool body_always_exits = false;
    if (IsTerminalFlow(body->flow_type()) && !stmt->has_continue())
        body_always_exits = true;

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

            // It's a bit tricky to merge this into the same heap scope as
            // the statement, so we create a one-off scope.
            if (advance->tree_has_heap_allocs())
                EnterHeapScope(Flow_None);

            EmitExpr(advance);

            if (advance->tree_has_heap_allocs())
                LeaveHeapScope();
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

    if (scope)
        debug_scope = {};
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

            case_labels.emplace(v.constval(), label);
        }

        EmitStmt(stmt);
        if (stmt->flow_type() == Flow_None)
            __ emit(OP_JUMP, &exit_label);
    }

    Label default_label;
    Label* defcase = &exit_label;
    if (stmt->default_case()) {
        __ bind(&default_label);

        EmitStmt(stmt->default_case());
        if (stmt->default_case()->flow_type() == Flow_None)
            __ emit(OP_JUMP, &exit_label);

        defcase = &default_label;
    }

    __ bind(&table_label);
    __ casetbl((int)case_labels.size(), defcase);

    for (auto& pair : case_labels)
        __ casetbl_entry(pair.first, &pair.second);

    __ bind(&exit_label);
}

void CodeGenerator::EmitFunctionDecl(FunctionDecl* info) {
    ke::SaveAndSet<FunctionDecl*> set_fun(&fun_, info);

    // Minimum 16 cells for general slack.
    current_memory_ = 16;
    max_func_memory_ = current_memory_;

    if (!info->is_live())
        return;

    if (info->canonical() == info)
        cc_.functions().emplace(info);

    if (!info->body())
        return;

    __ bind(&info->cg()->label);

    // Do this before we start crawling the body, since we need the method entry
    // to exist before we start emitting arg/local info.
    auto debug_method = AddFunctionEntry(info);

    __ emit(OP_PROC);
    AddDebugLine(info->pos());
    EmitBreak();
    current_stack_ = 0;
    locals_ = {};
    free_temp_slots_ = {};
    used_temp_slots_ = {};

    {
        AutoEnterScope arg_scope(this, &local_syms_);

        cell_t arg_index = 0;
        if (info->needs_hidden_arg())
            arg_index++;

        for (const auto& fun_arg : info->args()) {
            fun_arg->BindAddress(-(arg_index + 1));
            EnqueueDebugSymbol(fun_arg, asm_.position());
            arg_index++;
        }

        EmitStmt(info->body());
    }

    assert(!has_stack_or_heap_scopes());

    if (info->body()->flow_type() != Flow_Return) {
        __ emit(OP_ZERO_PRI);
        __ emit(OP_RETN);
    }

    __ emit(OP_ENDPROC);

    heap_scopes_.clear();

    info->cg()->pcode_end = asm_.pc();
    info->cg()->max_local_stack = max_func_memory_;

    // In case there is no callgraph, we still need to track which function has
    // the biggest stack.
    max_script_memory_ = std::max(max_script_memory_, max_func_memory_);

    if (debug_method)
        rtti_->finish_method(info, *debug_method, std::move(locals_));
    else
        assert(locals_.count == 0);
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
        EmitFunctionDecl(fun);
}

void
CodeGenerator::EmitMethodmapDecl(MethodmapDecl* decl)
{
    for (const auto& prop : decl->properties()) {
        if (prop->getter())
            EmitFunctionDecl(prop->getter());
        if (prop->setter())
            EmitFunctionDecl(prop->setter());
    }
    for (const auto& method : decl->methods())
        EmitFunctionDecl(method);
}

void CodeGenerator::EmitCall(FunctionDecl* fun, cell nargs) {
    assert(fun->is_live());

    if (fun->is_native()) {
        if (!fun->cg()->label.bound()) {
            cell index = AddNativeEntry(fun);
            __ bind_to(&fun->cg()->label, index);
        }
        __ sysreq_n(&fun->cg()->label, nargs);
    } else {
        __ emit(OP_PUSH_C, nargs);
        __ emit(OP_CALL, &fun->cg()->label);

        auto node = callgraph_.find(fun_);
        if (node == callgraph_.end())
            callgraph_.emplace(fun_, tr::vector<FunctionDecl*>{fun});
        else
            node->second.emplace_back(fun);
    }

    max_func_memory_ = std::max(max_func_memory_, current_memory_ + nargs);
}

void
CodeGenerator::EmitDefaultArray(Expr* expr, ArgDecl* arg)
{
    DefaultArg* def = arg->default_value();
    if (def->sym) {
        // Need to use the address label rather than raw address, since the
        // variable may not be emitted yet.
        __ emit(OP_CONST_PRI, def->sym->label());
        return;
    }

    if (!def->val) {
        def->val = ke::Some(data_.dat_address());

        data_.Add(std::move(def->array->iv));
        data_.Add(std::move(def->array->data));
        data_.AddZeroes(def->array->zeroes);
    }

    if (arg->type_info().is_const || !def->array) {
        // No modification is possible, so use the array we emitted. (This is
        // why we emitted the zeroes above.)
        __ const_pri(def->val.get());
    } else {
        cell iv_size = def->array->iv_size;
        cell data_size = def->array->data_size;
        cell total_size = iv_size + data_size + def->array->zeroes;

        //  heap <size>
        //  move.alt        ; pri = new address
        //  init.array
        //  move.alt        ; pri = new address
        __ emit(OP_HEAP, total_size * sizeof(cell));
        __ emit(OP_INITARRAY_ALT, def->val.get(), iv_size, data_size, def->array->zeroes, 0);
        __ emit(OP_MOVE_PRI);
        TrackTempHeapAlloc(expr, total_size);
    }
}

void CodeGenerator::EmitNumber64Expr(Number64Expr* expr) {
    Int64CellUnion u(*expr->ToInt64());

    auto slot = AcquireTempSlot(BuiltinType::Int64);
    __ emit(OP_STOR_S_C_I64, slot, u.cells[0], u.cells[1]);
    __ emit(OP_ADDR_PRI, slot);
}


void CodeGenerator::EmitSimpleCastExpr(SimpleCastExpr* expr) {
    EmitExpr(expr->from());

    Type* from_type = expr->from()->val().type();

    if (expr->to()->isInt64()) {
        assert(from_type->isInt() || from_type->isAny());
        auto slot = AcquireTempSlot(BuiltinType::Int64);
        __ emit(OP_CVT_I64, slot);
    } else if (expr->to()->isBool()) {
        if (from_type->isInt64())
            __ emit(OP_TEST_I64);
        else
            assert(false);
    } else {
        __ emit(OP_CVT_F32);
    }
}

void CodeGenerator::EmitCastExpr(CastExpr* expr) {
    auto from = expr->expr();
    EmitExpr(from);

    if (expr->val().type()->isInt() && from->val().type()->isInt64()) {
        __ emit(OP_TRUNCATE_I64);
    } else if (expr->val().type()->isInt64() && from->val().type()->isInt()) {
        auto slot = AcquireTempSlot(BuiltinType::Int64);
        __ emit(OP_CVT_I64, slot);
    }
}

void CodeGenerator::EmitFloatBuiltin(CallExpr* expr) {
    assert(expr->args().size() == 1);

    EmitExpr(expr->args()[0]);
    __ emit(OP_CVT_F32);
}

void
CodeGenerator::EnterMemoryScope(tr::vector<MemoryScope>& frame)
{
    if (frame.empty())
        frame.push_back(MemoryScope{0});
    else
        frame.push_back(MemoryScope{frame.back().scope_id + 1});
}

void
CodeGenerator::AllocInScope(ParseNode* node, MemoryScope& scope, MemuseType type, int size)
{
    if (type == MEMUSE_STATIC && !scope.usage.empty() && scope.usage.back().type == MEMUSE_STATIC) {
        scope.usage.back().size += size;
    } else {
        scope.usage.push_back(MemoryUse{type, size});
    }

    if (size > kMaxCells || current_memory_ + size >= kMaxCells) {
        report(node, 431);
        return;
    }

    current_memory_ += size;
    max_func_memory_ = std::max(current_memory_, max_func_memory_);
}

int
CodeGenerator::PopScope(tr::vector<MemoryScope>& scope_list)
{
    MemoryScope scope = ke::PopBack(&scope_list);
    int total_use = 0;
    while (!scope.usage.empty()) {
        assert(!errors_.ok() || scope.usage.back().size <= current_memory_);
        total_use += scope.usage.back().size;
        scope.usage.pop_back();
    }
    current_memory_ -= total_use;
    return total_use;
}

void CodeGenerator::TrackTempHeapAlloc(Expr* source, int size) {
    // Make sure the semantic pass determined that temporary allocations were necessary.
    assert(source->can_alloc_heap());
    TrackHeapAlloc(source, MEMUSE_STATIC, size);
}

void CodeGenerator::TrackHeapAlloc(ParseNode* node, MemuseType type, int size) {
    assert(!heap_scopes_.empty());
    AllocInScope(node, heap_scopes_.back(), type, size);
}

void CodeGenerator::EnterHeapScope(FlowType flow_type) {
    EnterMemoryScope(heap_scopes_);
    if (flow_type != Flow_Return) {
        heap_scopes_.back().needs_restore = true;
        __ emit(OP_HEAP_SAVE);
    }
}

void CodeGenerator::LeaveHeapScope() {
    assert(!heap_scopes_.empty());
    if (heap_scopes_.back().needs_restore)
        __ emit(OP_HEAP_RESTORE);
    heap_scopes_.pop_back();
}

int CodeGenerator::heap_scope_id() {
    if (heap_scopes_.empty())
        return -1;
    return heap_scopes_.back().scope_id;
}

int CodeGenerator::DynamicMemorySize() const {
    int min_cells = max_script_memory_;
    if (kMaxCells - 4096 > min_cells)
        min_cells = max_script_memory_ + 4096;

    int custom = cc_.options()->pragma_dynamic;
    return std::max(min_cells, custom) * sizeof(cell_t);
}

void CodeGenerator::EnqueueDebugSymbol(Decl* decl, uint32_t pc) {
    int vclass = 0;
    if (auto fun = decl->as<FunctionDecl>())
        vclass = fun->is_static() ? sSTATIC : sGLOBAL;
    else if (auto var = decl->as<VarDeclBase>())
        vclass = var->vclass();
    else
        assert(false);

    if (vclass == sGLOBAL) {
        global_syms_.emplace_back(decl, pc);
    } else if (vclass == sSTATIC && !fun_) {
        static_syms_.back().second.emplace_back(decl, pc);
    } else {
        local_syms_.back().emplace_back(decl, pc);
    }
}

CodeGenerator::AutoEnterScope::AutoEnterScope(CodeGenerator* cg, SymbolStack* scopes)
  : cg_(cg),
    scopes_(scopes)
{
    scopes_->emplace_back();
}

CodeGenerator::AutoEnterScope::~AutoEnterScope() {
    auto scope = ke::PopBack(scopes_);
    cg_->AddDebugSymbols(&scope);
}

bool CodeGenerator::ComputeStackUsage(CallGraph::iterator caller_iter) {
    FunctionDecl* caller = caller_iter->first;
    tr::vector<FunctionDecl*> targets = std::move(caller_iter->second);
    caller_iter = callgraph_.erase(caller_iter);

    int max_child_stack = 0;
    while (!targets.empty()) {
        FunctionDecl* target = ke::PopBack(&targets);
        if (!target->cg()->max_callee_stack) {
            auto iter = callgraph_.find(target);
            if (iter != callgraph_.end()) {
                if (!ComputeStackUsage(iter))
                    return false;
            }
        }

        auto local_stack = target->cg()->max_local_stack;
        auto callee_stack = target->cg()->max_callee_stack;
        if (!ke::IsUint32AddSafe(local_stack, callee_stack) ||
            local_stack + callee_stack >= kMaxCells)
        {
            report(token_pos_t{}, 431);
            return false;
        }

        max_child_stack = std::max(max_child_stack, local_stack + callee_stack);

        // Assign this each iteration so we at least have something useful if
        // we hit a recursive case.
        caller->cg()->max_callee_stack = max_child_stack;
    }

    auto local_stack = caller->cg()->max_local_stack;
    auto callee_stack = caller->cg()->max_callee_stack;
    if (!ke::IsUint32AddSafe(local_stack, callee_stack) ||
        local_stack + callee_stack >= kMaxCells)
    {
        report(token_pos_t{}, 431);
        return false;
    }

    max_script_memory_ = std::max(caller->cg()->max_local_stack +
                                  caller->cg()->max_callee_stack,
                                  max_script_memory_);
    return true;
}

bool CodeGenerator::ComputeStackUsage() {
    if (callgraph_.empty())
        return true;

    return ComputeStackUsage(callgraph_.begin());
}

cell_t CodeGenerator::AcquireTempSlot(BuiltinType builtin_type) {
    auto iter = free_temp_slots_.begin();
    while (iter != free_temp_slots_.end()) {
        if ((*iter).second == builtin_type) {
            used_temp_slots_.splice(used_temp_slots_.end(), free_temp_slots_, iter);
            return (*iter).first;
        }
        iter++;
    }

    auto type = cc_.types()->GetBuiltin(builtin_type);
    uint32_t slot = rtti_->AddLocalSlot(&locals_, QualType(type));
    used_temp_slots_.emplace_back(slot, builtin_type);
    return slot;
}

std::optional<smx_rtti_debug_method> CodeGenerator::AddFunctionEntry(FunctionDecl* fun) {
    if (fun->is_native())
        return {};
    if (!fun->body())
        return {};
    if (!fun->is_live())
        return {};
    if (fun->canonical() != fun)
        return {};

    uint32_t name_idx;
    if (fun->is_public()) {
        name_idx = names_->add(fun->name());
    } else {
        auto temp_name = ke::StringPrintf(".%d.%s", fun->cg()->label.offset(), fun->name()->chars());
        name_idx = names_->add(*cc_.atoms(), temp_name);
    }

    assert(fun->cg()->label.offset() > 0);
    assert(fun->impl());

    sp_file_publics_t& pubfunc = publics_->add();
    pubfunc.address = fun->cg()->label.offset();
    pubfunc.name = name_idx;

    auto id = (uint32_t(publics_->count() - 1) << 1) | 1;
    if (!Label::ValueFits(id))
        report(421);
    __ bind_to(&fun->cg()->funcid, id);

    return {rtti_->add_method(fun)};
}

uint32_t CodeGenerator::AddNativeEntry(FunctionDecl* decl) {
    sp_file_natives_t& entry = natives_->add();
    entry.name = names_->add(decl->name());

    rtti_->add_native(decl);
    return natives_->count() - 1;
}

} // namespace cc
} // namespace sp
