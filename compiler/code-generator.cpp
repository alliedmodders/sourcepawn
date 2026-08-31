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
    pubvars_ = new SmxPubvarSection(".pubvars");
    code_ = new SmxCodeSection(".code");
    rtti_ = std::make_unique<RttiBuilder>(cc, names_);
}

bool CodeGenerator::Generate() {
    // We always have at least one instruction.
    __ emit(OP_NOP);

    EmitStmtList(tree_->stmts());

    for (const auto& ctor : tree_->global_ctors())
        EmitFunctionDecl(ctor);

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
    smx_.add(pubvars_);
    smx_.add(names_);
    rtti_->finish(smx_);
}

void CodeGenerator::AddDebugLine(const token_pos_t& pos) {
    if (!fun_)
        return;
		
    auto line = cc_.sources()->GetLineAndCol(pos, nullptr);
    auto method = rtti_->GetMethod(debug_info_.method_index);
    uint32_t rel_addr = asm_.position() - method.pcode_start;
    uint32_t rel_line = (uint32_t)line - debug_info_.line_start;

    if (rel_addr < UINT16_MAX && rel_line < UINT16_MAX)
        rtti_->AddDebugLine((uint16_t)rel_addr, (uint16_t)rel_line);
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

void CodeGenerator::EmitStmtList(StmtList* list) {
    for (const auto& stmt : list->stmts()) {
        EmitStmt(stmt);

        // Trying to emit unreachable statements is buggy for some reason, so
        // just don't do it.
        if (stmt->flow_type() != Flow_None)
            break;
    }
}

void CodeGenerator::EmitStmt(Stmt* stmt) {
    std::list<std::pair<uint32_t, BuiltinType>> prev_used_temp_slots;

    if (fun_) {
        AddDebugLine(stmt->pos());

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
            EmitExpr(stmt->to<ExprStmt>()->expr(), EMIT_DISCARD_RESULT);
            break;
        case StmtKind::BlockStmt: {
            auto s = stmt->to<BlockStmt>();

            {
                AutoEnterScope locals(this, &local_syms_);
                EmitStmtList(s);
            }
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
        case StmtKind::GlobalInitStmt:
            EmitGlobalInitStmt(stmt->to<GlobalInitStmt>());
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
    __ bind_to(decl->label(), data_.dat_address());

    if (decl->type()->isArray() || decl->type()->isEnumStruct()) {
        ArrayData array;
        BuildCompoundInitializer(decl, &array, data_.dat_address());

        data_.Add(std::move(array.iv));
        data_.Add(std::move(array.data));
        data_.AddZeroes(array.zeroes);
    } else {
        cell_t cells = 1;
        if (auto es = decl->type()->asEnumStruct())
            cells = es->array_size();
        else if (decl->type()->isInt64())
            cells = 2;

        data_.AddZeroes(cells);
    }
}

void CodeGenerator::EmitGlobalInitStmt(GlobalInitStmt* stmt) {
    for (const auto& var : stmt->vars()) {
        auto init = var->init();

        AddDebugLine(init->pos());

        if (auto n64 = init->right()->as<Number64Expr>()) {
            __ emit(OP_PUSH_C_I64, Int64Value(*n64->ToInt64()));
            __ emit(OP_STOR_GLB_I64, var->label());
        } else {
            assert(init->right()->val().ident == iCONSTEXPR);
            __ PUSH_C(init->right()->val().constval());
            __ emit(OP_STOR_GLB, var->label());
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
    if (slot > INT16_MAX)
        report(decl->pos(), 467);
    decl->BindAddress(slot);

    if (!is_array && !is_struct) {
        if (init) {
            const auto& val = init->right()->val();
            if (val.ident == iCONSTEXPR) {
                __ emit(OP_STOR_S_C, StackSlot(slot), val.constval());
            } else if (auto n64 = init->right()->as<Number64Expr>()) {
                Int64CellUnion u(*n64->ToInt64());
                __ emit(OP_STOR_S_C_I64, StackSlot(slot), u.cells[0], u.cells[1]);
            } else {
                EmitExpr(init->right());
                if (num_cells == 1)
                    __ emit(OP_STOR_S, StackSlot(slot));
                else if (num_cells == 2)
                    __ emit(OP_STOR_S_I64, StackSlot(slot));
                else
                    assert(false);
            }
        } else if (num_cells == 2) {
            __ emit(OP_ZERO_S_I64, StackSlot(slot));
        } else if (num_cells == 1) {
            // Note: we no longer honor "decl" for scalars.
            __ emit(OP_ZERO_S, StackSlot(slot));
        }
    } else {
        auto init_rhs = decl->init_rhs();
        if (init_rhs && init_rhs->as<NewArrayExpr>()) {
            EmitExpr(init_rhs->as<NewArrayExpr>());
            __ emit(OP_STOR_S, StackSlot(slot));
        } else if (!init_rhs || decl->type()->isArray() || is_struct) {
            ArrayData array;
            BuildCompoundInitializer(decl, &array, 0);

            cell iv_size = (cell)array.iv.size();
            cell data_size = (cell)array.data.size() + array.zeroes;
            cell total_size = iv_size + data_size;

            max_array_size_ = std::max(max_array_size_, total_size);

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
            __ emit(OP_DUP);
            __ emit(OP_INITARRAY, iv_addr, iv_size, non_filled, array.zeroes, 0);
            __ emit(OP_STOR_S, StackSlot(slot));
        } else if (StringExpr* ctor = init_rhs->as<StringExpr>()) {
            auto queue_size = data_.size();
            auto str_addr = data_.dat_address();
            data_.Add(ctor->text()->chars(), ctor->text()->length());

            auto cells = data_.size() - queue_size;
            assert(cells > 0);

            __ PUSH_C(cells);
            if (decl->autozero())
                __ emit(OP_GENARRAY_Z, 1);
            else
                __ emit(OP_GENARRAY, 1);
            __ emit(OP_DUP);
            __ PUSH_C(str_addr);
            __ emit(OP_MOVS, cells * sizeof(cell));
            __ emit(OP_STOR_S, StackSlot(decl->addr()));
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

void CodeGenerator::EmitExpr(Expr* expr, unsigned int flags) {
    AutoErrorPos aep(expr->pos());

    if (expr->val().ident == iCONSTEXPR) {
        if (!(flags & EMIT_DISCARD_RESULT))
            __ PUSH_C(expr->val().constval());
        return;
    }

    switch (expr->kind()) {
        case ExprKind::UnaryExpr:
            EmitUnary(expr->to<UnaryExpr>());
            break;
        case ExprKind::IncDecExpr:
            EmitIncDec(expr->to<IncDecExpr>(), flags);
            break;
        case ExprKind::BinaryExpr:
            EmitBinary(expr->to<BinaryExpr>(), flags);
            break;
        case ExprKind::LogicalExpr:
            EmitLogicalExpr(expr->to<LogicalExpr>());
            break;
        case ExprKind::ChainedCompareExpr:
            EmitChainedCompareExpr(expr->to<ChainedCompareExpr>());
            break;
        case ExprKind::TernaryExpr:
            EmitTernaryExpr(expr->to<TernaryExpr>(), flags);
            break;
        case ExprKind::CastExpr:
            EmitCastExpr(expr->to<CastExpr>());
            break;
        case ExprKind::SymbolExpr:
            EmitSymbolExpr(expr->to<SymbolExpr>());
            break;
        case ExprKind::RvalueExpr: {
            EmitRvalue(expr->to<RvalueExpr>());
            break;
        }
        case ExprKind::CommaExpr: {
            EmitCommaExpr(expr->to<CommaExpr>(), flags);
            break;
        }
        case ExprKind::ThisExpr: {
            auto e = expr->to<ThisExpr>();
            if (e->decl()->type()->isEnumStruct())
                __ address(e->decl());
            break;
        }
        case ExprKind::StringExpr: {
            auto se = expr->to<StringExpr>();
            auto addr = data_.dat_address();
            data_.Add(se->text()->chars(), se->text()->length());
            __ PUSH_C(addr);
            break;
            }
            case ExprKind::ArrayExpr: {
            auto e = expr->to<ArrayExpr>();

            auto addr = data_.dat_address();
            for (const auto& expr : e->exprs())
                data_.Add(expr->val().constval());
            __ PUSH_C(addr);
            break;
            }
        case ExprKind::IndexExpr:
            EmitIndexExpr(expr->to<IndexExpr>());
            break;
        case ExprKind::FieldAccessExpr:
            EmitFieldAccessExpr(expr->to<FieldAccessExpr>());
            break;
        case ExprKind::CallExpr:
            EmitCallExpr(expr->to<CallExpr>(), flags);
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

    if ((flags & EMIT_DISCARD_RESULT) && !expr->HandlesDiscardResult())
        __ emit(OP_POP);
}

bool Expr::HandlesDiscardResult() {
    switch (kind()) {
        case ExprKind::IncDecExpr:
        case ExprKind::CommaExpr:
        case ExprKind::CallExpr:
        case ExprKind::TernaryExpr:
            return true;
        case ExprKind::BinaryExpr:
            return IsAssignOp(to<BinaryExpr>()->token());
        default:
            return val().ident == iCONSTEXPR;
    }
}

void CodeGenerator::EmitTest(Expr* expr, bool jump_on_true, Label* target) {
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
            if (inner->val().type()->isInt64())
                __ emit(OP_INVERT_I64);
            else
                __ emit(OP_INVERT);
            break;
        case '!':
            if (inner->val().type()->isInt64())
                __ emit(OP_TEST_I64);
            else if (inner->val().type()->isFloat())
                __ emit(OP_TEST_F32);
            __ emit(OP_NOT);
            break;
        case '-':
            if (inner->val().type()->isInt64())
                __ emit(OP_NEG_I64);
            else if (inner->val().type()->isFloat())
                __ emit(OP_NEG_F32);
            else
                __ emit(OP_NEG);
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

void CodeGenerator::EmitIncDec(IncDecExpr* expr, unsigned int flags) {
    EmitExpr(expr->expr());

    const auto& val = expr->expr()->val();

    Type* type = val.type();
    if (type->isReference())
        type = type->inner();

    // Save base address if needed.
    if (!val.canRematerialize())
        __ emit(OP_DUP);

    EmitRvalue(val);

    bool discard = !!(flags & EMIT_DISCARD_RESULT);

    if (!expr->prefix() && !discard) {
        if (!val.canRematerialize())
            __ emit(OP_DUP_ROTATE);
        else
            __ emit(OP_DUP);
        // Stack is now either [PRE_VAL, ADDR, PRE_VAL] or [PRE_VAL, PRE_VAL]
    }

    if (type->isInt64()) {
        __ PUSH_C((expr->token() == tINC) ? 1 : -1);
        __ emit(OP_CVT_I64);
        __ emit(OP_ADD_I64);
    } else if (type->isFloat()) {
        float val = (expr->token() == tINC ? 1.0f : -1.0f);
        __ PUSH_C(sp_ftoc(val));
        __ emit(OP_ADD_F32);
    } else {
        __ emit(expr->token() == tINC ? OP_INC : OP_DEC);
    }

    if (expr->prefix() && !discard) {
        if (!val.canRematerialize())
            __ emit(OP_DUP_ROTATE);
        else
            __ emit(OP_DUP);
        // Stack is now either [POST_VAL, ADDR, POST_VAL] or [POST_VAL, POST_VAL]
    }

    EmitStore(expr, val);
}

void CodeGenerator::EmitBinary(BinaryExpr* expr, unsigned int flags) {
    auto left = expr->left();
    const auto& left_val = left->val();

    auto right = expr->right();

    auto token = expr->token();
    auto oper = NormalizeBinaryToken(token);

    EmitExpr(left);

    if (IsAssignOp(token)) {
        // assign-modify needs the base address twice (load, store).
        if (!left_val.canRematerialize() && oper)
            __ emit(OP_DUP);

        if (oper)
            EmitRvalue(left_val);

        if (expr->array_copy_length()) {
            assert(!oper);

            EmitExpr(right);
            if (!(flags & EMIT_DISCARD_RESULT))
                __ emit(OP_DUP_ROTATE);
            __ emit(OP_MOVS, expr->array_copy_length() * sizeof(cell));
            return;
        }
    }

    assert(!expr->array_copy_length());
    assert(!left_val.type()->isArray());

    EmitBinaryInner(expr, oper, left, right);

    if (IsAssignOp(token)) {
        if (!(flags & EMIT_DISCARD_RESULT)) {
            // Stack is either [base val] or [val].
            if (!left_val.canRematerialize() && oper)
                __ emit(OP_DUP_ROTATE);
            else
                __ emit(OP_DUP);
            // Stack is now either [val base val] or [val val].
        }
        EmitStore(expr, left_val);
    }
}

void CodeGenerator::EmitBinaryInner(Expr* expr, int oper_tok, Expr* left, Expr* right, bool save) {
    EmitExpr(right);

    if (save)
        __ emit(OP_DUP_ROTATE);

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
        case '/': return OP_DIV_F32;
        case '%': return OP_MOD_F32;
        case '+': return OP_ADD_F32;
        case '-': return OP_SUB_F32;
        case tlEQ: return OP_EQ_F32;
        case tlNE: return OP_NEQ_F32;
        case '>': return OP_GRTR_F32;
        case tlGE: return OP_GEQ_F32;
        case '<': return OP_LESS_F32;
        case tlLE: return OP_LEQ_F32;
        default:
            assert(false);
            return OP_NOP;
    }
}

OPCODE GetInt32BinaryOp(int oper_tok) {
    switch (oper_tok) {
        case '*': return OP_SMUL;
        case '/': return OP_SDIV_I32;
        case '%': return OP_SMOD_I32;
        case '+': return OP_ADD;
        case '-': return OP_SUB;
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
            return OP_NOP;
    }
}

OPCODE GetInt64BinaryOp(int oper_tok) {
    switch (oper_tok) {
        case '*': return OP_SMUL_I64;
        case '/': return OP_SDIV_I64;
        case '%': return OP_SMOD_I64;
        case '+': return OP_ADD_I64;
        case '-': return OP_SUB_I64;
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
            return OP_NOP;
    }
}

void CodeGenerator::EmitBinaryOp(Expr* expr, BuiltinType type, int oper_tok) {
    if (type == BuiltinType::Int64) {
        __ emit(GetInt64BinaryOp(oper_tok));
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
    __ PUSH_C((cell_t)!jump_on_true);
    __ emit(OP_JUMP, &done);
    __ bind(&shortcircuit);
    __ PUSH_C((cell_t)jump_on_true);
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
            return OP_NOP;
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
    EmitExpr(right);

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

void CodeGenerator::EmitChainedCompareExpr(ChainedCompareExpr* root) {
    Label on_false, last_false, done;
    EmitExpr(root->first());

    Expr* left = root->first();

    assert(root->ops().size() > 0);
    for (size_t i = 0; i < root->ops().size(); i++) {
        const auto& op = root->ops().at(i);
        int oper_tok = NormalizeBinaryToken(op.token);
        if (i != root->ops().size() - 1) {
            EmitBinaryInner(root, oper_tok, left, op.expr, true /* save */);
            __ emit(OP_JZER, &on_false);
        } else {
            EmitBinaryInner(root, oper_tok, left, op.expr, false /* save */);
            __ emit(OP_JZER, &last_false);
        }
        left = op.expr;
    }

    __ PUSH_C(1);
    __ emit(OP_JUMP, &done);
    __ bind(&on_false);
    __ emit(OP_POP);
    __ bind(&last_false);
    __ PUSH_C(0);
    __ bind(&done);
}

void CodeGenerator::EmitTernaryExpr(TernaryExpr* expr, unsigned int flags) {
    EmitExpr(expr->first());

    Label flab1, flab2;

    __ emit(OP_JZER, &flab1);
    EmitExpr(expr->second(), flags);
    __ emit(OP_JUMP, &flab2);
    __ bind(&flab1);
    EmitExpr(expr->third(), flags);
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

        __ emit(OP_LOAD_FN, &fun->cg()->method_id);
    } else if (auto var = sym->as<VarDeclBase>()) {
        if (sym->type()->isComposite())
            __ address(var);
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

    EmitExpr(expr->index());

    uint32_t bounds = array_type->size() ? array_type->size() : INT_MAX;
    __ idxaddr(rank_size, bounds);

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
        if (field->offset())
            __ emit(OP_ADD_C, field->offset() << 2);
    }
}

void CodeGenerator::EmitCallExpr(CallExpr* call, unsigned int flags) {
    auto return_type = call->fun()->return_type();
    bool discard = !!(flags & EMIT_DISCARD_RESULT);

    if (call->fun()->is_builtin()) {
        auto iter = builtins_.find(call->fun()->name());
        assert(iter != builtins_.end());

        (this->*(iter->second))(call);

        if (discard && !return_type->isVoid())
            __ emit(OP_POP);
        return;
    }

    // Calculate the hidden parameter if needed. If we need to heap allocate,
    // we store the address in a local slot, so we can easily read it back out
    // after the function returns. For simple stack allocations we just use a
    // local variable.
    cell_t nargs = (cell_t)call->args().size();

    const auto& argv = call->args();
    const auto& arginfov = call->fun()->args();
    for (size_t i = argv.size() - 1; i < argv.size(); i--) {
        const auto& expr = argv[i];

        EmitExpr(expr);

        if (expr->as<DefaultArgExpr>())
            continue;

        const auto& val = expr->val();
        bool lvalue = expr->lvalue();

        ArgDecl* arg;
        if (i < arginfov.size()) {
            arg = arginfov[i];
        } else {
            arg = arginfov.back();
            assert(arg->type_info().is_varargs);
        }

        bool needs_temp = false;
        if (arg->type_info().is_varargs) {
            if (val.ident == iVARIABLE && !val.type()->isComposite()) {
                assert(val.sym);
                assert(lvalue);
                /* treat a "const" variable passed to a function with a non-const
                 * "variable argument list" as a constant here */
                if (val.sym->is_const() && !arg->type_info().is_const) {
                    EmitRvalue(val);
                    needs_temp = true;
                } else if (lvalue) {
                    __ address(val.sym);
                } else {
                    needs_temp = true;
                }
            } else if (val.ident == iCONSTEXPR || val.ident == iEXPRESSION) {
                needs_temp = !val.type()->isComposite();
            }
            if (needs_temp) {
                if (val.type()->isInt64()) {
                    auto slot = AcquireTempSlot(expr, BuiltinType::Int64);
                    __ emit(OP_STOR_S_I64, StackSlot(slot));
                    __ emit(OP_ADDR_S, StackSlot(slot));
                } else {
                    auto slot = AcquireTempSlot(expr, BuiltinType::Int);
                    __ emit(OP_STOR_S, StackSlot(slot));
                    __ emit(OP_ADDR_S, StackSlot(slot));
                }
            }
        } else if (arg->type_info().type->isReference()) {
            if (val.ident == iVARIABLE && !val.type()->isComposite())
                __ address(val.sym);
        }

        // Always pass int64s by reference, as a hack for backward compatibility
        // with natives and GetLocalParams.
        if (arg->type_info().type->isInt64()) {
            assert(val.type()->isInt64());

            auto slot = AcquireTempSlot(expr, BuiltinType::Int64);
            __ emit(OP_STOR_S_I64, StackSlot(slot));
            __ emit(OP_ADDR_S, StackSlot(slot));
        }
    }

    std::optional<uint32_t> hidden_slot;

    if (call->fun()->needs_hidden_arg()) {
        if (return_type->isArray()) {
            EmitCallHiddenArray(call);

            hidden_slot = {AcquireTempSlot(call, BuiltinType::Int)};
            __ emit(OP_DUP);
            __ emit(OP_STOR_S, StackSlot(*hidden_slot));
        } else if (return_type->isEnumStruct()) {
            cell retsize = return_type->CellStorageSize();
            assert(retsize);

            hidden_slot = {AcquireTempSlot(call, BuiltinType::Int)};
            __ emit(OP_HEAP, retsize * sizeof(cell));
            __ emit(OP_DUP);
            __ emit(OP_STOR_S, StackSlot(*hidden_slot));
        } else {
            assert(return_type->isInt64());

            hidden_slot = {AcquireTempSlot(call, BuiltinType::Int64)};
            __ emit(OP_ADDR_S, StackSlot(*hidden_slot));
        }
        nargs++;
    }

    EmitCall(call->fun(), nargs);

    if (discard) {
        if (!return_type->isVoid())
            __ emit(OP_POP);
    } else if (hidden_slot) {
        if (return_type->isArray() || return_type->isEnumStruct())
            __ emit(OP_LOAD_S, StackSlot(*hidden_slot));
        else
            __ emit(OP_LOAD_S_I64, StackSlot(*hidden_slot));
    }
}

void CodeGenerator::EmitCallHiddenArray(CallExpr* call) {
    auto fun = call->fun();

    ArrayData array;
    BuildCompoundInitializer(QualType(fun->return_type()), nullptr, &array);

    cell retsize = call->fun()->return_type()->CellStorageSize();
    assert(retsize);

    __ emit(OP_HEAP, retsize * sizeof(cell));
    __ emit(OP_DUP);

    auto info = fun->return_array();
    if (array.iv.empty()) {
        __ PUSH_C(0);
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

        __ emit(OP_INITARRAY, dat_addr, iv_size, 0, info->zeroes, 0);
    }
}

void
CodeGenerator::EmitDefaultArgExpr(DefaultArgExpr* expr)
{
    const auto& arg = expr->arg();
    assert(!arg->type()->isInt64());

    if (arg->type()->isReference()) {
        auto temp_slot = AcquireTempSlot(expr, BuiltinType::Int);
        __ PUSH_C(arg->default_value()->val.get());
        __ emit(OP_STOR_S, StackSlot(temp_slot));
        __ emit(OP_ADDR_S, StackSlot(temp_slot));
    } else if (arg->type()->isArray()) {
        EmitDefaultArray(expr, arg);
    } else {
        if (arg->type()->isEnumStruct())
            EmitDefaultArray(expr, arg);
        else
            __ PUSH_C(arg->default_value()->val.get());
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
                __ emit(OP_STRADJUST);
            } else if (auto es = innermost->asEnumStruct(); es && es->array_size() > 1) {
                __ emit(OP_SMUL_C, es->array_size());
            }
        }
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
        if (stmt->flow_type() == Flow_None)
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
        EmitExpr(stmt->expr());

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

    // Get the data address of the source array.
    EmitExpr(stmt->expr());
    __ emit(OP_ADD_C, iv_size * sizeof(cell));

    // Initialize the dest aray.
    __ load_hidden_arg(fun_);
    __ emit(OP_DUP);
    __ emit(OP_INITARRAY, dat_addr, iv_size, 0, 0, 0);
    __ emit(OP_ADD_C, iv_size * sizeof(cell));
    __ emit(OP_SWAP);
    __ emit(OP_MOVS, info->zeroes * sizeof(cell));

    __ load_hidden_arg(fun_);
}

void
CodeGenerator::EmitReturnStmt(ReturnStmt* stmt)
{
    if (stmt->expr()) {
        const auto& v = stmt->expr()->val();
        if (v.type()->isArray() || v.type()->isEnumStruct()) {
            EmitReturnArrayStmt(stmt);
            __ emit(OP_RETV);
        } else if (v.type()->isInt64()) {
            // Must copy to the hidden arg.
            __ load_hidden_arg(fun_);
            EmitExpr(stmt->expr());
            __ emit(OP_STOR_I_I64);
            __ emit(OP_RETV);
        } else {
            EmitExpr(stmt->expr());
            __ emit(OP_RETN);
        }
    } else {
        /* this return statement contains no expression */
        __ emit(OP_RETV);
    }
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

    if (expr->lvalue()) {
        if (zap && v.ident == iACCESSOR && !v.accessor()->setter())
            zap = false;

        if (zap && !v.canRematerialize())
            __ emit(OP_DUP);

        EmitRvalue(v);
    }

    EmitCall(stmt->map()->dtor(), 1);

    if (zap) {
        // Store 0 back.
        __ PUSH_C(0);
        EmitStore(stmt, v);
    }
}

void CodeGenerator::EmitRvalue(RvalueExpr* expr) {
    auto lval = expr->lval();
    switch (lval->val().ident) {
        case iARRAYCELL:
        case iARRAYCHAR:
        case iACCESSOR:
            EmitExpr(expr->lval());
            break;
        case iVARIABLE:
            break;
        default:
            assert(false);
    }
    EmitRvalue(lval->val());
}

void CodeGenerator::EmitRvalue(const value& lval) {
    switch (lval.ident) {
        case iARRAYCELL:
            if (lval.type()->isInt64())
                __ emit(OP_LOAD_I_I64);
            else if (!lval.type()->isComposite())
                __ emit(OP_LOAD_I);
            break;
        case iARRAYCHAR:
            __ emit(OP_LODB_I);
            break;
        case iACCESSOR:
            InvokeGetter(lval.accessor());
            break;
        case iVARIABLE: {
            if (lval.type()->isReference()) {
                auto var = lval.sym->as<VarDeclBase>();
                assert(var->vclass() == sLOCAL || var->vclass() == sARGUMENT);
                if (lval.type()->inner()->isInt64()) {
                    // int64 arguments are passed by-ref for compatibility.
                    __ emit(OP_LOAD_S, StackSlot(var->addr()));
                    __ emit(OP_LOAD_I_I64);
                } else {
                    __ emit(OP_LREF_S, StackSlot(var->addr()));
                }
                break;
            }
            [[fallthrough]];
        }
        default: {
            auto var = lval.sym->as<VarDeclBase>();
            if (var->vclass() == sLOCAL || var->vclass() == sARGUMENT) {
                if (var->type()->isInt64()) {
                    if (var->vclass() == sLOCAL) {
                        __ emit(OP_LOAD_S_I64, StackSlot(var->addr()));
                    } else {
                        // int64 arguments are passed by-ref for compatibility.
                        __ emit(OP_LOAD_S, StackSlot(var->addr()));
                        __ emit(OP_LOAD_I_I64);
                    }
                } else {
                    __ emit(OP_LOAD_S, StackSlot(var->addr()));
                }
            } else if (!var->type()->isComposite()) {
                if (var->type()->isInt64())
                    __ emit(OP_LOAD_GLB_I64, var->label());
                else
                    __ emit(OP_LOAD_GLB, var->label());
            } else {
                __ emit(OP_PUSH_C, var->label());
            }
            break;
        }
    }
}

void CodeGenerator::EmitStore(ParseNode* pn, const value& lval) {
    switch (lval.ident) {
        case iARRAYCELL:
            if (lval.type()->isInt64())
                __ emit(OP_STOR_I_I64);
            else
                __ emit(OP_STOR_I);
            break;
        case iARRAYCHAR:
            __ emit(OP_STRB_I);
            break;
        case iACCESSOR:
            if (lval.type()->isInt64()) {
                // Need to pass the int64 as an address for native compatibility.
                auto slot = AcquireTempSlot(pn, BuiltinType::Int64);
                __ emit(OP_STOR_S_I64, StackSlot(slot));
                __ emit(OP_ADDR_S, StackSlot(slot));
            }
            // Calls have their arguments in reverse order, so we have to swap
            // the top of the stack.
            __ emit(OP_SWAP);
            EmitCall(lval.accessor()->setter(), 2);
            break;
        case iVARIABLE: {
            if (lval.type()->isReference()) {
                auto var = lval.sym->as<VarDeclBase>();
                assert(var->vclass() == sLOCAL || var->vclass() == sARGUMENT);

                if (lval.type()->inner()->isInt64()) {
                    __ emit(OP_LOAD_S, StackSlot(var->addr()));
                    __ emit(OP_SWAP);
                    __ emit(OP_STOR_I_I64);
                } else {
                    __ emit(OP_SREF_S, StackSlot(var->addr()));
                }
                break;
            }
            [[fallthrough]];
        }
        default: {
            auto var = lval.sym->as<VarDeclBase>();
            if (var->vclass() == sLOCAL || var->vclass() == sARGUMENT) {
                if (var->type()->isInt64())
                    __ emit(OP_STOR_S_I64, StackSlot(var->addr()));
                else
                    __ emit(OP_STOR_S, StackSlot(var->addr()));
            } else {
                if (var->type()->isInt64())
                    __ emit(OP_STOR_GLB_I64, var->addr());
                else
                    __ emit(OP_STOR_GLB, var->addr());
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
        hidden_slot = {AcquireTempSlot(prop, BuiltinType::Int64)};

    cell_t nargs = 1;
    if (hidden_slot) {
        __ emit(OP_ADDR_S, StackSlot(*hidden_slot));
        nargs++;
    }

    EmitCall(prop->getter(), nargs);

    if (hidden_slot)
        __ emit(OP_LOAD_S_I64, StackSlot(*hidden_slot));
}

void CodeGenerator::EmitDoWhileStmt(DoWhileStmt* stmt) {
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
                __ PUSH_C(0);
                __ emit(OP_JUMP, &join);
                __ bind(&on_true);
                __ PUSH_C(1);
                __ bind(&join);
                LeaveHeapScope();
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
            __ PUSH_C(0);
            __ emit(OP_JUMP, &join);
            __ bind(&on_true);
            __ PUSH_C(1);
            __ bind(&join);
            LeaveHeapScope();
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

            EmitExpr(advance, EMIT_DISCARD_RESULT);

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

    if (!info->is_live())
        return;

    if (info->canonical() == info)
        cc_.functions().emplace(info);

    if (!info->body())
        return;

    // Do this before we start crawling the body, since we need the method entry
    // to exist before we start emitting arg/local info.
    debug_info_ = AddFunctionEntry(info, asm_.pc());

    AddDebugLine(info->pos());
    locals_ = {};
    free_temp_slots_ = {};
    used_temp_slots_ = {};

    {
        AutoEnterScope arg_scope(this, &local_syms_);

        cell_t arg_index = 0;
        if (info->needs_hidden_arg())
            arg_index++;

        for (const auto& fun_arg : info->args()) {
            int32_t offset = -(arg_index + 1);
            if (offset < INT16_MIN)
                report(fun_arg->pos(), 467);
            fun_arg->BindAddress(offset);
            EnqueueDebugSymbol(fun_arg, asm_.position());
            arg_index++;
        }

        EmitStmt(info->body());
    }

    assert(!has_stack_or_heap_scopes());

    if (info->body()->flow_type() != Flow_Return) {
        if (info->MustReturnValue()) {
            __ PUSH_C(0);
            __ emit(OP_RETN);
        } else {
            __ emit(OP_RETV);
        }
    }

    heap_scopes_.clear();

    uint32_t pcode_end = asm_.pc();

    rtti_->finish_method(info, debug_info_, std::move(locals_), pcode_end);
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
        if (!fun->cg()->method_id.bound()) {
            auto entry = rtti_->add_method(fun, 0);
            rtti_->finish_method(fun, entry, LocalSlotSignature{}, 0);

            __ bind_to(&fun->cg()->method_id, entry.method_index);
        }
    } else {
        auto node = callgraph_.find(fun_);
        if (node == callgraph_.end())
            callgraph_.emplace(fun_, tr::vector<FunctionDecl*>{fun});
        else
            node->second.emplace_back(fun);
    }

    if (fun->IsVariadic()) {
        __ emit(OP_CALLN, &fun->cg()->method_id, static_cast<uint8_t>(nargs));
    } else {
        __ emit(OP_CALL, &fun->cg()->method_id);
    }
}

void
CodeGenerator::EmitDefaultArray(Expr* expr, ArgDecl* arg)
{
    DefaultArg* def = arg->default_value();
    if (def->sym) {
        // Need to use the address label rather than raw address, since the
        // variable may not be emitted yet.
        // :TODO: If we switch _GLB opcodes to use RTTI, this will need to
        // change.
        __ emit(OP_PUSH_C, def->sym->label());
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
        __ PUSH_C(def->val.get());
    } else {
        cell iv_size = def->array->iv_size;
        cell data_size = def->array->data_size;
        cell total_size = iv_size + data_size + def->array->zeroes;

        //  heap <size>
        //  move.alt        ; pri = new address
        //  init.array
        //  move.alt        ; pri = new address
        __ emit(OP_HEAP, total_size * sizeof(cell));
        __ emit(OP_DUP);
        __ emit(OP_INITARRAY, def->val.get(), iv_size, data_size, def->array->zeroes, 0);
    }
}

void CodeGenerator::EmitNumber64Expr(Number64Expr* expr) {
    Int64CellUnion u(*expr->ToInt64());

    auto slot = AcquireTempSlot(expr, BuiltinType::Int64);
    __ emit(OP_STOR_S_C_I64, StackSlot(slot), u.cells[0], u.cells[1]);
    __ emit(OP_LOAD_S_I64, StackSlot(slot));
}


void CodeGenerator::EmitSimpleCastExpr(SimpleCastExpr* expr) {
    EmitExpr(expr->from());

    Type* from_type = expr->from()->val().type();

    if (expr->to()->isInt64()) {
        assert(from_type->isInt() || from_type->isAny());
        __ emit(OP_CVT_I64);
    } else if (expr->to()->isBool()) {
        if (from_type->isInt64())
            __ emit(OP_TEST_I64);
        else
            assert(false);
    } else {
        __ emit(OP_CVT_F32);
    }
}

static inline bool CoercesToInt64(Type* type) {
    return type->isInt() || type->isAny();
}

void CodeGenerator::EmitCastExpr(CastExpr* expr) {
    auto from = expr->expr();
    EmitExpr(from);

    if (CoercesToInt64(expr->val().type()) && from->val().type()->isInt64()) {
        __ emit(OP_TRUNCATE_I64);
    } else if (expr->val().type()->isInt64() && CoercesToInt64(from->val().type())) {
        __ emit(OP_CVT_I64);
    }
}

void CodeGenerator::EmitCommaExpr(CommaExpr* ce, unsigned int flags) {
    for (const auto& expr : ce->exprs()) {
        unsigned int new_flags;
        if (expr == ce->exprs().back())
            new_flags = flags;
        else
            new_flags = EMIT_DISCARD_RESULT;
        EmitExpr(expr, new_flags);
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
    int custom = cc_.options()->pragma_dynamic;
    int max_array = std::max(4096, max_array_size_ * 4);
    return std::max(max_array, custom) * sizeof(cell_t);
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

cell_t CodeGenerator::AcquireTempSlot(ParseNode* node, BuiltinType builtin_type) {
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
    if (slot > INT16_MAX)
        report(node->pos(), 467);
    used_temp_slots_.emplace_back(slot, builtin_type);
    return slot;
}

smx_rtti_debug_method CodeGenerator::AddFunctionEntry(FunctionDecl* fun, uint32_t pcode_start) {
    assert(!fun->is_native());
    assert(fun->body());
    assert(fun->is_live());
    assert(fun->canonical() == fun);
    assert(fun->impl());

    auto debug_method = rtti_->add_method(fun, pcode_start);
    __ bind_to(&fun->cg()->method_id, debug_method.method_index);
    return {debug_method};
}

} // namespace cc
} // namespace sp
