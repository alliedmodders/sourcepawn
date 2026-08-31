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
#include "utils/compact-encoding.h"

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
    std::list<std::pair<uint32_t, Type*>> prev_used_temp_slots;

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
}

void CodeGenerator::EmitGlobalVar(VarDeclBase* decl) {
    Atom* name = decl->name();
    if (decl->vclass() == sSTATIC && fun_)
        name = cc_.atom(fun_->name()->str() + "." + name->str());

    if (!decl->label()->bound()) {
        uint32_t index = rtti_->AddGlobal(decl, name);
        if (index > UINT16_MAX)
            report(decl, 468);

        __ bind_to(decl->label(), index);
    } else {
        // We already bound this global earlier, but we need to update its name.
        uint16_t index = decl->label()->offset();
        rtti_->UpdateGlobalName(index, name);
    }

    decl->set_is_emitted();
}

uint16_t CodeGenerator::AcquireGlobalSlot(VarDeclBase* decl) {
    if (decl->label()->bound())
        return decl->label()->offset();

    uint32_t index = rtti_->AddGlobal(decl, nullptr);
    if (index > UINT16_MAX)
        report(decl, 468);

    __ bind_to(decl->label(), index);
    return index;
}

void CodeGenerator::EmitGlobalInitStmt(GlobalInitStmt* stmt) {
    for (const auto& var : stmt->vars()) {
        auto init = var->init_rhs();

        if (!var->is_emitted())
            continue;

        if (init)
            AddDebugLine(init->pos());

        if (auto array = var->type()->as<ArrayType>()) {
            if (array->is_flat()) {
                if (!init)
                    continue;
                __ emit(OP_ADDR_GLB, VarSlot(var->addr()));
            }
            EmitArrayCtor(array, init, 0);
            if (!array->is_flat())
                __ emit(OP_STOR_GLB, VarSlot(var->addr()));
        } else if (var->type()->isEnumStruct()) {
            if (!init)
                continue;
            __ emit(OP_ADDR_GLB, VarSlot(var->addr()));
            EmitEnumStructCtor(var->type()->asEnumStruct(), init);
        } else if (init && init->as<Number64Expr>()) {
            auto n64 = init->as<Number64Expr>();
            __ emit(OP_PUSH_C_I64, Int64Value(*n64->ToInt64()));
            __ emit(OP_STOR_GLB, VarSlot(var->addr()));
        } else if (init && init->val().ident == iCONSTEXPR) {
            __ PUSH_C(init->val().constval());
            __ emit(OP_STOR_GLB, VarSlot(var->addr()));
        } else if (init) {
            assert(false);
        }
    }
}

static inline uint32_t DeduceArraySize(ArrayType* type, Expr* ctor) {
    if (type->is_fixed())
        return type->size();
    if (auto array = ctor->as<ArrayExpr>())
        return (uint32_t)array->exprs().size();
    if (auto se = ctor->as<StringExpr>())
        return se->text()->str().size() + 1;

    assert(false);
    return 0;
}

void CodeGenerator::EmitArrayExpr(ArrayExpr* expr, unsigned int flags) {
    auto type = expr->val().type()->as<ArrayType>();

    if (type->is_flat()) {
        auto temp_slot = AcquireTempSlot(expr, type);
        __ emit(OP_ADDR_S, VarSlot(temp_slot));
        EmitArrayCtor(type, expr, flags);
        __ emit(OP_ADDR_S, VarSlot(temp_slot));
    } else {
        EmitArrayCtor(type, expr, flags);
    }
}

void CodeGenerator::EmitArrayCtor(ArrayType* type, Expr* ctor, unsigned int flags) {
    if (auto new_array = Expr::As<NewArrayExpr>(ctor)) {
        assert(!type->is_flat());
        EmitNewArrayExpr(new_array);
        return;
    }

    if (!type->is_flat()) {
        // The array has not been allocated yet.
        uint32_t type_id = rtti_->to_typeid(type);
        if (!type->is_fixed()) {
            uint32_t size = DeduceArraySize(type, ctor);
            __ PUSH_C(size);
        }
        __ emit(OP_NEWARRAY, type_id);
    } else {
        // Otherwise, the address has been pushed onto the stack by the caller.
    }

    if (type->inner()->isEnumStruct()) {
        ArrayExpr* array = ctor ? ctor->to<ArrayExpr>() : nullptr;

        for (size_t i = 0; i < array->exprs().size(); i++) {
            __ emit(OP_DUP);
            __ PUSH_C(i);
            __ emit(OP_IDXADDR);

            if (array)
                EmitEnumStructCtor(type->inner()->asEnumStruct(), array->exprs().at(i));
        }

        if (type->is_flat())
            __ emit(OP_POP);
    } else if (ArrayType* inner = type->inner()->as<ArrayType>()) {
        assert(!inner->is_flat());
        ArrayExpr* array = ctor ? ctor->to<ArrayExpr>() : nullptr;

        uint32_t len = array ? (uint32_t)array->exprs().size() : type->size();
        for (size_t i = 0; i < len; i++) {
            __ emit(OP_DUP);
            __ PUSH_C(i);
            __ emit(OP_IDXADDR);

            // If the inner array is flat, then it's already been allocated.
            if (inner->is_flat())
                __ emit(OP_LOAD_I_I32);

            EmitArrayCtor(inner, array ? array->exprs().at(i) : nullptr, 0);

            // Otherwise, the allocation is now on the stack.
            if (!inner->is_flat())
                __ emit(OP_STOR_I_I32);
        }

        // No longer need the parent address.
        if (type->is_flat())
            __ emit(OP_POP);
    } else if (ctor) {
        uint32_t fill_data_pos;

        auto iter = fill_data_cache_.find(ctor);
        if (iter != fill_data_cache_.end()) {
            fill_data_pos = iter->second;
        } else if (auto array = ctor->as<ArrayExpr>()) {
            fill_data_pos = EmitArrayFillData(type, array);
        } else if (auto str = ctor->as<StringExpr>()) {
            fill_data_pos = EmitStringFillData(type, str);
        } else {
            assert(false);
            return;
        }

        if (flags & EMIT_REPEATABLE)
            fill_data_cache_.emplace(ctor, fill_data_pos);

        // If this is a flat array, the address was pushed onto the stack by our
        // caller, and now we're consuming it. Otherwise, the caller expects the
        // address to be returned on the stack.
        if (!type->is_flat())
            __ emit(OP_DUP);
        __ emit(OP_FILLARRAY, fill_data_pos);
    }
}

template <typename T>
static inline void AddValue(std::string* out, T value) {
    union {
        T value;
        char bytes[sizeof(T)];
    } u;
    u.value = value;
    out->append(u.bytes, sizeof(u.bytes));
}

void CodeGenerator::EmitEnumStructCtor(EnumStructDecl* es, Expr* ctor) {
    ArrayExpr* array = ctor ? ctor->to<ArrayExpr>() : nullptr;
    if (!array)
        return;

    const auto& field_list = es->fields();
    auto field_iter = field_list.begin();

    for (size_t i = 0; i < field_list.size(); i++) {
        auto field = *field_iter;
        field_iter++;

        if (i >= array->exprs().size())
            break;

        Expr* expr = array->exprs().at(i);
        auto field_type = field->type_info().type;
        __ emit(OP_DUP);
        if (auto field_array = field_type->as<ArrayType>()) {
            uint32_t ref = rtti_->AddFieldRef(field);
            __ emit(OP_ADDR_FLD, ref);
            EmitArrayCtor(field_array, expr, 0);
        } else if (auto field_es = field_type->asEnumStruct()) {
            uint32_t ref = rtti_->AddFieldRef(field);
            __ emit(OP_ADDR_FLD, ref);
            EmitEnumStructCtor(field_es, expr);
        } else {
            EmitExpr(expr);
            uint32_t ref = rtti_->AddFieldRef(field);
            __ emit(OP_STOR_FLD, ref);
        }
    }

    // Pop the base address from the stack
    __ emit(OP_POP);
}

uint32_t CodeGenerator::EmitArrayFillData(ArrayType* type, ArrayExpr* array) {
    std::string data;

    uint32_t num_items = 0;
    std::optional<cell_t> prev1, prev2;
    for (const auto& item : array->exprs()) {
        prev2 = prev1;
        if (auto n64 = item->as<Number64Expr>()) {
            AddValue<int64_t>(&data, *n64->ToInt64());
            prev1 = {};
        } else {
            assert(item->val().ident == iCONSTEXPR);
            cell_t cv = item->val().constval();
            if (type->inner()->isInt64())
                AddValue<int64_t>(&data, cv);
            else
                AddValue<int32_t>(&data, cv);
            prev1 = {cv};
        }
        num_items++;
    }

    // If we have ellipses, it should be a fixed array.
    assert(!array->ellipses() || type->size());

    if (array->ellipses() && num_items < type->size()) {
        cell_t step = 0;
        if (prev2)
            step = *prev1 - *prev2;

        cell_t next_value = *prev1 + step;
        while (num_items < type->size()) {
            if (type->inner()->isInt64())
                AddValue<int64_t>(&data, next_value);
            else
                AddValue<int32_t>(&data, next_value);
            next_value += step;
            num_items++;
        }
    }

    std::string prefix;
    if (!EncodeCompactUint32(&prefix, data.size())) {
        report(array, 431);
        return 0;
    }

    uint32_t pos = data_.dat_address();
    data_.Add(prefix);
    data_.Add(data);
    return pos;
}

uint32_t CodeGenerator::EmitStringFillData(ArrayType* type, StringExpr* array) {
    assert(type->inner()->isChar());

    auto text = array->text();

    std::string prefix;
    if (!EncodeCompactUint32(&prefix, text->str().size())) {
        report(array, 431);
        return 0;
    }

    uint32_t pos = data_.dat_address();
    data_.Add(prefix);
    data_.Add(text->str());
    return pos;
}

void CodeGenerator::EmitLocalVar(VarDeclBase* decl) {
    BinaryExpr* init = decl->init();

    bool is_struct = decl->type()->isEnumStruct();

    int num_cells;
    if (decl->type()->isBuiltin(BuiltinType::Int64))
        num_cells = 2;
    else
        num_cells = 1;

    int32_t slot = rtti_->AddLocalSlot(&locals_, decl->type());
    if (slot > INT16_MAX)
        report(decl->pos(), 467);
    decl->BindAddress(slot);

    auto init_rhs = decl->init_rhs();
    if (auto array = decl->type()->as<ArrayType>()) {
        if (array->is_flat()) {
            if (!init_rhs)
                return;
            __ emit(OP_ADDR_S, VarSlot(slot));
        }
        EmitArrayCtor(array, init_rhs, 0);
        if (!array->is_flat())
            __ emit(OP_STOR_S, VarSlot(slot));
    } else if (is_struct) {
        if (init_rhs) {
            __ emit(OP_ADDR_S, VarSlot(slot));
            EmitEnumStructCtor(decl->type()->asEnumStruct(), init_rhs);
        }
    } else {
        if (init) {
            const auto& val = init->right()->val();
            if (val.ident == iCONSTEXPR) {
                __ emit(OP_STOR_S_C, VarSlot(slot), val.constval());
            } else if (auto n64 = init->right()->as<Number64Expr>()) {
                __ emit(OP_PUSH_C_I64, Int64Value(*n64->ToInt64()));
                __ emit(OP_STOR_S, VarSlot(slot));
            } else {
                EmitExpr(init->right());
                __ emit(OP_STOR_S, VarSlot(slot));
                assert(num_cells == 1 || num_cells == 2);
            }
        } else if (num_cells == 1) {
            __ emit(OP_STOR_S_C, VarSlot(slot), 0);
        } else if (num_cells == 2) {
            __ emit(OP_PUSH_C_I64, Int64Value(0));
            __ emit(OP_STOR_S, VarSlot(slot));
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
        if (!(flags & EMIT_DISCARD_RESULT)) {
            if (expr->val().type()->isFloat())
                __ emit(OP_PUSH_C_F32, expr->val().constval());
            else
                __ PUSH_C(expr->val().constval());
        }
        return;
    }

    assert(!expr->lvalue() || !!(flags & EMIT_ALLOW_LVALUE));

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
            EmitCastExpr(expr->to<CastExpr>(), flags);
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
                EmitAddress(e->decl());
            break;
        }
        case ExprKind::StringExpr: {
            auto se = expr->to<StringExpr>();
            uint16_t index = rtti_->AddString(se->text(), &data_);
            __ emit(OP_LOAD_STR, VarSlot(index));
            break;
        }

        case ExprKind::ArrayExpr:
            EmitArrayExpr(expr->to<ArrayExpr>(), flags);
            break;
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
        case ExprKind::SliceExpr:
            EmitSliceExpr(expr->to<SliceExpr>());
            break;
        case ExprKind::SizeofExpr:
            EmitSizeofExpr(expr->to<SizeofExpr>(), flags);
            break;

        default:
            assert(false);
    }

    if ((flags & EMIT_DISCARD_RESULT) && !expr->HandlesDiscardResult())
        __ emit(OP_POP);
}

void CodeGenerator::EmitSizeofExpr(SizeofExpr* expr, unsigned int flags) {
    Expr* child = expr->child();
    const auto& cv = child->val();
    EnumStructDecl* es = nullptr;

    switch (cv.ident) {
        case iARRAYELEM:
        case iVARIABLE:
        case iEXPRESSION:
            es = cv.type()->asEnumStruct();
            break;
        case iTYPENAME:
            es = cv.typename_decl()->as<EnumStructDecl>();
            break;
        default:
            break;
    }

    assert(es != nullptr);
    uint32_t type_id = rtti_->to_typeid(es->type());
    __ emit(OP_LOAD_ES_SIZE, type_id);
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
            __ emit(OP_INVERT);
            break;
        case '!':
            if (inner->val().type()->isInt64() || inner->val().type()->isFloat())
                __ emit(OP_TEST);
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
    if (expr->token() == '!') {
        auto inner = expr->expr();
        if (!inner->val().type()->isInt64()) {
            EmitTest(expr->expr(), !jump_on_true, target);
            return true;
        }
    }
    return false;
}

value CodeGenerator::BindLvalue(Expr* expr, bool simple_address) {
    value val = expr->val();
    switch (val.ident) {
        case iVARIABLE:
            break;
        case iARRAYELEM:
            EmitExpr(expr, EMIT_ALLOW_LVALUE);
            // Array types are loaded as addresses by OP_LOAD_ELEM_A and do not need OP_IDXADDR.
            if (simple_address && !val.type()->isArray()) {
                __ emit(OP_IDXADDR);
                val.ident = iADDRESS;
            }
            break;
        case iACCESSOR:
            EmitExpr(expr, EMIT_ALLOW_LVALUE);
            break;
        case iFIELD: {
            auto fe = expr->as<FieldAccessExpr>();
            EmitExpr(fe->base());
            break;
        }
        case iADDRESS: {
            EmitExpr(expr, EMIT_ALLOW_LVALUE);
            break;
        }
        default:
            assert(false);
    }
    return val;
}

void CodeGenerator::EmitIncDec(IncDecExpr* expr, unsigned int flags) {
    bool discard = !!(flags & EMIT_DISCARD_RESULT);
    value val = BindLvalue(expr->expr(), true);

    Type* type = val.type();
    if (type->isReference())
        type = type->inner();

    // Save base address if needed.
    if (!val.canRematerialize())
        __ emit(OP_DUP);

    EmitRvalue(val);

    // We use a temporary to store the result value, if we need to due to the
    // l-value mucking up the operand stack.
    std::optional<uint32_t> temp_slot;

    if (!expr->prefix() && !discard) {
        temp_slot = {AcquireTempSlot(expr, type)};
        __ emit(OP_DUP);
        __ emit(OP_STOR_S, VarSlot(*temp_slot));
    }

    __ emit(expr->token() == tINC ? OP_INC : OP_DEC);

    if (expr->prefix() && !discard) {
        __ emit(OP_DUP);
        if (!val.canRematerialize()) {
            temp_slot = {AcquireTempSlot(expr, type)};
            __ emit(OP_STOR_S, VarSlot(*temp_slot));
        }
    }

    EmitStore(expr, val);

    if (temp_slot)
        __ emit(OP_LOAD_S, VarSlot(*temp_slot));
}

[[maybe_unused]] static inline bool StackSlotsForLval(const value& v) {
    switch (v.ident) {
        case iVARIABLE:
            return 0;
        case iACCESSOR:
        case iADDRESS:
        case iEXPRESSION:
        case iFIELD:
            return 1;
        case iARRAYELEM:
            return 2;
        default:
            assert(false);
            return 0;
    }
}

void CodeGenerator::EmitBinary(BinaryExpr* expr, unsigned int flags) {
    auto left = expr->left();
    auto right = expr->right();

    auto token = expr->token();
    auto oper = NormalizeBinaryToken(token);
    bool discard = !!(flags & EMIT_DISCARD_RESULT);

    if (expr->enum_struct_copy()) {
        EmitRvalueFromLvalue(left);

        assert(IsAssignOp(token));
        assert(!oper);

        EmitExpr(right);
        auto es = left->val().type()->asEnumStruct();
        assert(es != nullptr);
        uint32_t type_id = rtti_->to_typeid(es->type());
        __ emit(OP_COPYOBJ, type_id);
        return;
    }

    if (expr->array_copy()) {
        EmitRvalueFromLvalue(left);

        assert(IsAssignOp(token));
        assert(!oper);

        EmitExpr(right);
        __ emit(OP_COPYARRAY);
        return;
    }

    value left_val;
    if (IsAssignOp(token)) {
        left_val = BindLvalue(left, !!oper);

        if (oper) {
            assert(StackSlotsForLval(left_val) <= 1);

            // assign-modify needs the base address twice (load, store).
            if (!left_val.canRematerialize())
                __ emit(OP_DUP);

            EmitRvalue(left_val);
        }
    } else {
        EmitExpr(left);
        left_val = left->val();
    }

    assert(!expr->array_copy());
    assert(!left_val.type()->isArray());

    EmitExpr(right);
    EmitBinaryTail(expr, oper, left, right);

    if (IsAssignOp(token)) {
        std::optional<uint32_t> temp_slot;

        if (!discard) {
            // Stack is one of the following cases.
            //   iVARIABLE:
            //      [val]
            //   iARRAYELEM: (implies !oper)
            //      [base, index, val]
            //   iADDRESS: (implies oper)
            //      [base, val]
            //   iACCESSOR:
            //   iEXPRESSION:
            //      [base, val]
            //
            // Since we have !discard, we need to preserve the calculated value,
            // which we do via a local if there is too much stack manipulation
            // involved.
            __ emit(OP_DUP);
            if (!left_val.canRematerialize()) {
                auto temp_type = expr->val().type();
                temp_slot = {AcquireTempSlot(expr, temp_type)};
                __ emit(OP_STOR_S, VarSlot(*temp_slot));
            }
        }
        EmitStore(expr, left_val);
        if (temp_slot)
            __ emit(OP_LOAD_S, VarSlot(*temp_slot));
    }
}

void CodeGenerator::EmitBinaryTail(Expr* expr, int oper_tok, Expr* left, Expr* right) {
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
        case '*': return OP_SMUL;
        case '/': return OP_SDIV;
        case '%': return OP_SMOD;
        case '+': return OP_ADD;
        case '-': return OP_SUB;
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

OPCODE GetInt32BinaryOp(int oper_tok) {
    switch (oper_tok) {
        case '*': return OP_SMUL;
        case '/': return OP_SDIV;
        case '%': return OP_SMOD;
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
        case '*': return OP_SMUL;
        case '/': return OP_SDIV;
        case '%': return OP_SMOD;
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

    std::unordered_map<Type*, uint32_t> temp_slots;

    assert(root->ops().size() > 0);
    for (size_t i = 0; i < root->ops().size(); i++) {
        const auto& op = root->ops().at(i);
        int oper_tok = NormalizeBinaryToken(op.token);

        EmitExpr(op.expr);

        std::optional<uint32_t> temp_slot;
        if (i != root->ops().size() - 1) {
            auto temp_type = op.expr->val().type();
            if (auto iter = temp_slots.find(temp_type); iter != temp_slots.end()) {
                temp_slot = {iter->second};
            } else {
                temp_slot = {AcquireTempSlot(op.expr, temp_type)};
                temp_slots.emplace(temp_type, *temp_slot);
            }

            __ emit(OP_DUP);
            __ emit(OP_STOR_S, VarSlot(*temp_slot));
        }

        EmitBinaryTail(root, oper_tok, left, op.expr);
        __ emit(OP_JZER, &on_false);

        if (temp_slot)
            __ emit(OP_LOAD_S, VarSlot(*temp_slot));
        left = op.expr;
    }

    __ PUSH_C(1);
    __ emit(OP_JUMP, &done);
    __ bind(&on_false);
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
        if (sym->type()->isCompositeValue())
            EmitAddress(var);
    } else {
        assert(false);
    }
}

void CodeGenerator::EmitIndexExpr(IndexExpr* expr) {
    EmitExpr(expr->base());
    EmitExpr(expr->index());

    auto& base_val = expr->base()->val();
    auto array_type = base_val.type()->as<ArrayType>();

    // The indexed item is another array (multi-dimensional arrays).
    if (array_type->inner()->isArray()) {
        assert(expr->val().type()->isArray());
        __ emit(OP_LOAD_ELEM_A);
    }
}

void CodeGenerator::EmitSliceExpr(SliceExpr* slice) {
    if (slice->expr()->lvalue()) {
        EmitRvalueFromLvalue(slice->expr());
    } else {
        EmitExpr(slice->expr());
    }

    auto es = slice->expr()->val().type()->asEnumStruct();
    if (es) {
        uint32_t type_id = rtti_->to_typeid(es->type());
        __ emit(OP_SLICE_ES, type_id);
    } else if (slice->expr()->val().type()->isArray() && !slice->index()) {
        uint32_t type_id = rtti_->to_typeid(slice->val().type());
        __ emit(OP_SLICE_AS, type_id);
    } else {
        if (slice->index())
            EmitExpr(slice->index());
        else
            __ PUSH_C(0);
        __ emit(OP_SLICE);
    }
}

bool CodeGenerator::IsElidableSlice(Expr* expr, FunctionDecl* fun, ArgDecl* arg) {
    if (!fun->is_native())
        return false;
    if (expr->kind() != ExprKind::SliceExpr)
        return false;
    if (arg->type_info().is_varargs)
        return false;
    if (!arg->type_info().type->isFlatArray())
        return false;
    return expr->to<SliceExpr>()->expr()->val().type()->isFlatArray();
}

void CodeGenerator::EmitElidedSliceExpr(SliceExpr* slice) {
    if (slice->expr()->lvalue()) {
        EmitRvalueFromLvalue(slice->expr());
    } else {
        EmitExpr(slice->expr());
    }
    if (slice->index()) {
        EmitExpr(slice->index());
        __ emit(OP_IDXADDR);
    }
}

void CodeGenerator::EmitFieldAccessExpr(FieldAccessExpr* expr) {
    if (expr->token() == tDBLCOLON) {
        LayoutFieldDecl* field = expr->resolved()->as<LayoutFieldDecl>();
        uint32_t ref = rtti_->AddFieldRef(field);
        __ emit(OP_LOAD_FLD_OFFSET, ref);
        return;
    }

    assert(expr->token() == '.');

    // Note that we do not load an iACCESSOR here, we only make sure the base
    // is computed. Emit() never performs loads on l-values, that ability is
    // reserved for RvalueExpr().
    EmitExpr(expr->base());

    // Only enum struct accesses have a resolved decl.
    if (!expr->resolved())
        return;

    assert(false);
}

static inline Type* UnwrapRef(Type* type) {
    if (type->isReference())
        return type->inner();
    return type;
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

        ArgDecl* arg;
        if (i < arginfov.size()) {
            arg = arginfov[i];
        } else {
            arg = arginfov.back();
            assert(arg->type_info().is_varargs);
        }

        // Don't generate "slice ; array2native" sequences on local arrays,
        // since "slice" and "array2native" cancel each other out.
        bool is_elided_slice = IsElidableSlice(expr, call->fun(), arg);
        if (is_elided_slice) {
            EmitElidedSliceExpr(expr->to<SliceExpr>());
        } else {
            bool lvalue = expr->lvalue();
            if (lvalue)
                BindLvalue(expr, true);
            else
                EmitExpr(expr);
        }

        if (expr->as<DefaultArgExpr>())
            continue;

        const auto& val = expr->val();

        bool needs_temp = false;
        if (arg->type_info().is_varargs) {
            bool lvalue = expr->lvalue();
            if (val.ident == iVARIABLE && !val.type()->isComposite()) {
                assert(val.sym());
                assert(lvalue);
                /* treat a "const" variable passed to a function with a non-const
                 * "variable argument list" as a constant here */
                if (val.sym()->is_const() && !arg->type_info().is_const)
                    needs_temp = true;
            } else if (val.ident == iCONSTEXPR || val.ident == iEXPRESSION) {
                needs_temp = !val.type()->isComposite();
            }

            if (lvalue) {
                if (needs_temp)
                    EmitRvalue(val);
                else if (val.ident == iVARIABLE)
                    EmitAddress(val.sym());
                else if (val.ident == iFIELD)
                    EmitAddress(val);
            }

            if (needs_temp) {
                auto slot = AcquireTempSlot(expr, UnwrapRef(val.type()));
                __ emit(OP_STOR_S, VarSlot(slot));
                __ emit(OP_ADDR_S, VarSlot(slot));
            }
        } else if (arg->type_info().type->isReference()) {
            if (val.ident == iVARIABLE && !val.type()->isComposite())
                EmitAddress(val.sym());
        }

        // Always pass int64s by reference, as a hack for backward compatibility
        // with natives and GetLocalParams.
        if (arg->type_info().type->isInt64()) {
            assert(val.type()->isInt64());

            auto slot = AcquireTempSlot(expr, BuiltinType::Int64);
            __ emit(OP_STOR_S, VarSlot(slot));
            __ emit(OP_ADDR_S, VarSlot(slot));
        }

        if (val.type()->isArray() && !val.type()->isFlatArray() && call->fun()->is_native() && !is_elided_slice)
            __ emit(OP_ARRAY_TO_NATIVE);
    }

    std::optional<uint32_t> hidden_slot;

    if (call->fun()->needs_hidden_arg()) {
        if (return_type->isCompositeValue()) {
            auto slot = AcquireTempSlot(call, return_type);
            __ emit(OP_ADDR_S, VarSlot(slot));
            hidden_slot = {slot};
        } else if (auto type = return_type->as<ArrayType>()) {
            assert(!type->is_flat());
            auto slot = AcquireTempSlot(call, type);
            EmitArrayCtor(type, nullptr, 0);
            __ emit(OP_STOR_S, VarSlot(slot));
            __ emit(OP_LOAD_S, VarSlot(slot));
            hidden_slot = {slot};
        } else {
            assert(return_type->isInt64());

            hidden_slot = {AcquireTempSlot(call, BuiltinType::Int64)};
            __ emit(OP_ADDR_S, VarSlot(*hidden_slot));
        }
        nargs++;
    }

    EmitCall(call->fun(), nargs);

    if (discard) {
        if (!return_type->isVoid())
            __ emit(OP_POP);
    } else if (hidden_slot) {
        if (return_type->isCompositeValue()) {
            __ emit(OP_ADDR_S, VarSlot(*hidden_slot));
        } else {
            __ emit(OP_LOAD_S, VarSlot(*hidden_slot));
        }
    }
}

void CodeGenerator::EmitDefaultArgExpr(DefaultArgExpr* expr) {
    const auto& arg = expr->arg();
    assert(!arg->type()->isInt64());

    auto init = arg->init_rhs();

    if (auto array = init->as<ArrayExpr>()) {
        Type* type = *arg->type();
        if (type->isEnumStruct()) {
            auto temp_slot = AcquireTempSlot(expr, type);
            __ emit(OP_ADDR_S, VarSlot(temp_slot));
            EmitEnumStructCtor(type->asEnumStruct(), array);

            __ emit(OP_ADDR_S, VarSlot(temp_slot));
        } else {
            auto arr_type = type->as<ArrayType>();
            if (arr_type->is_flat()) {
                auto temp_slot = AcquireTempSlot(expr, arr_type);
                __ emit(OP_ADDR_S, VarSlot(temp_slot));
                EmitArrayCtor(arr_type, array, EMIT_REPEATABLE);

                __ emit(OP_ADDR_S, VarSlot(temp_slot));
            } else {
                EmitArrayCtor(arr_type, array, EMIT_REPEATABLE);
            }
        }
    } else {
        if (init->lvalue())
            EmitRvalue(init->val());
        else
            EmitExpr(init);
        if (arg->type()->isReference()) {
            auto temp_slot = AcquireTempSlot(expr, arg->type()->inner());
            __ emit(OP_STOR_S, VarSlot(temp_slot));
            __ emit(OP_ADDR_S, VarSlot(temp_slot));
        }
    }
}

void CodeGenerator::EmitNewArrayExpr(NewArrayExpr* expr) {
    uint32_t type_id = rtti_->to_typeid(expr->type());
    const auto& exprs = expr->exprs();

    // Find the number of dynamic dimensions leading up to the first fixed
    // dimension.
    size_t num_dynamic = 0;
    ArrayType* type = expr->type()->as<ArrayType>();
    while (type && !type->is_fixed()) {
        num_dynamic++;
        type = type->inner()->as<ArrayType>();
    }

    // Emit these onto the stack.
    assert(num_dynamic <= exprs.size());
    for (size_t i = num_dynamic - 1; i < num_dynamic; i--)
        EmitExpr(exprs[i]);

    if (num_dynamic > std::numeric_limits<uint8_t>::max())
        report(expr, 431);

    if (num_dynamic <= 1)
        __ emit(OP_NEWARRAY, type_id);
    else
        __ newbulkarray((uint8_t)num_dynamic, type_id);
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
    if (auto es = fun_->return_type()->asEnumStruct()) {
        __ load_hidden_arg(fun_);
        EmitExpr(stmt->expr());
        uint32_t type_id = rtti_->to_typeid(es->type());
        __ emit(OP_COPYOBJ, type_id);
        __ emit(OP_RETV);
        return;
    }

    auto type = fun_->return_type()->as<ArrayType>();

    if (type->inner()->isArray()) {
        EmitExpr(stmt->expr());
        __ emit(OP_JUMP, &ret_2d_array_);
    } else {
        __ load_hidden_arg(fun_);
        EmitExpr(stmt->expr());
        __ emit(OP_COPYARRAY);
        __ emit(OP_RETV);
    }
}

void
CodeGenerator::EmitReturnStmt(ReturnStmt* stmt)
{
    if (stmt->expr()) {
        const auto& v = stmt->expr()->val();
        if (v.type()->isArray() || v.type()->isEnumStruct()) {
            EmitReturnArrayStmt(stmt);
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
    } else if (fun_->MustReturnValue() || !fun_->return_type()->isVoid()) {
        __ PUSH_C(0);
        __ emit(OP_RETN);
    } else {
        /* this return statement contains no expression */
        __ emit(OP_RETV);
    }
}

void
CodeGenerator::EmitDeleteStmt(DeleteStmt* stmt)
{
    Expr* expr = stmt->expr();
    value v = expr->val();

    // Only zap non-const lvalues.
    bool zap = expr->lvalue();
    if (zap) {
        if (v.ident == iVARIABLE && v.sym()->is_const())
            zap = false;
        else if (v.ident == iACCESSOR && !v.accessor()->setter())
            zap = false;
    }

    if (expr->lvalue()) {
        v = BindLvalue(expr, true);

        if (zap && !v.canRematerialize())
            __ emit(OP_DUP);

        EmitRvalue(v);
    } else {
        EmitExpr(expr);
    }

    EmitCall(stmt->map()->dtor(), 1);

    if (zap) {
        // Store 0 back.
        __ PUSH_C(0);
        EmitStore(stmt, v);
    }
}

void CodeGenerator::EmitRvalue(RvalueExpr* expr) {
    EmitRvalueFromLvalue(expr->lval());
}

void CodeGenerator::EmitRvalueFromLvalue(Expr* expr) {
    assert(expr->lvalue());
    value val = BindLvalue(expr);
    EmitRvalue(val);
}

void CodeGenerator::EmitRvalue(const value& lval) {
    switch (lval.ident) {
        case iARRAYELEM:
            assert(!lval.type()->isFlatArray());
            if (lval.type()->isChar())
                __ emit(OP_LOAD_ELEM_U8);
            else if (lval.type()->isInt64())
                __ emit(OP_LOAD_ELEM_I64);
            else if (lval.type()->isFloat())
                __ emit(OP_LOAD_ELEM_F32);
            else if (!lval.type()->isComposite())
                __ emit(OP_LOAD_ELEM_I32);
            else if (lval.type()->isCompositeValue())
                __ emit(OP_IDXADDR);
            else if (!lval.type()->isArray())
                assert(false);
            break;
        case iADDRESS:
            if (lval.type()->isComposite())
                break;
            if (lval.type()->isChar())
                __ emit(OP_LOAD_I_U8);
            else if (lval.type()->isInt64())
                __ emit(OP_LOAD_I_I64);
            else if (lval.type()->isFloat())
                __ emit(OP_LOAD_I_F32);
            else
                __ emit(OP_LOAD_I_I32);
            break;
        case iFIELD: {
            auto field = lval.field();
            uint32_t ref = rtti_->AddFieldRef(field);
            if (lval.type()->isCompositeValue())
                __ emit(OP_ADDR_FLD, ref);
            else
                __ emit(OP_LOAD_FLD, ref);
            break;
        }
        case iACCESSOR:
            InvokeGetter(lval.accessor());
            break;
        case iVARIABLE: {
            if (lval.type()->isReference()) {
                auto var = lval.sym();
                assert(var->vclass() == sLOCAL || var->vclass() == sARGUMENT);
                __ emit(OP_LOAD_S, VarSlot(var->addr()));
                if (lval.type()->inner()->isInt64())
                    // int64 arguments are passed by-ref for compatibility.
                    __ emit(OP_LOAD_I_I64);
                else if (lval.type()->inner()->isFloat())
                    __ emit(OP_LOAD_I_F32);
                else
                    __ emit(OP_LOAD_I_I32);
                break;
            }
            [[fallthrough]];
        }
        default: {
            auto var = lval.sym();
            if (var->vclass() == sLOCAL || var->vclass() == sARGUMENT) {
                if (var->type()->isInt64() && var->vclass() == sARGUMENT) {
                    // int64 arguments are passed by-ref for compatibility.
                    __ emit(OP_LOAD_S, VarSlot(var->addr()));
                    __ emit(OP_LOAD_I_I64);
                } else if (var->type()->isCompositeValue()) {
                    EmitAddress(var);
                } else {
                    __ emit(OP_LOAD_S, VarSlot(var->addr()));
                }
            } else {
                uint16_t slot = AcquireGlobalSlot(var);
                if (var->type()->isCompositeValue())
                    __ emit(OP_ADDR_GLB, VarSlot(slot));
                else
                    __ emit(OP_LOAD_GLB, VarSlot(slot));
            }
            break;
        }
    }
}

void CodeGenerator::EmitStore(ParseNode* pn, const value& lval) {
    switch (lval.ident) {
        case iARRAYELEM:
            if (lval.type()->isChar())
                __ emit(OP_STOR_ELEM_U8);
            else if (lval.type()->isInt64())
                __ emit(OP_STOR_ELEM_I64);
            else if (lval.type()->isFloat())
                __ emit(OP_STOR_ELEM_F32);
            else
                __ emit(OP_STOR_ELEM_I32);
            assert(!lval.type()->isComposite());
            break;
        case iADDRESS:
            if (lval.type()->isChar())
                __ emit(OP_STOR_I_U8);
            else if (lval.type()->isInt64())
                __ emit(OP_STOR_I_I64);
            else if (lval.type()->isFloat())
                __ emit(OP_STOR_I_F32);
            else
                __ emit(OP_STOR_I_I32);
            assert(!lval.type()->isComposite());
            break;
        case iFIELD: {
            auto field = lval.field();
            uint32_t ref = rtti_->AddFieldRef(field);
            __ emit(OP_STOR_FLD, ref);
            break;
        }
        case iACCESSOR:
            if (lval.type()->isInt64()) {
                // Need to pass the int64 as an address for native compatibility.
                auto slot = AcquireTempSlot(pn, BuiltinType::Int64);
                __ emit(OP_STOR_S, VarSlot(slot));
                __ emit(OP_ADDR_S, VarSlot(slot));
            }
            // Calls have their arguments in reverse order, so we have to swap
            // the top of the stack.
            __ emit(OP_SWAP);
            EmitCall(lval.accessor()->setter(), 2);
            break;
        case iVARIABLE: {
            if (lval.type()->isReference()) {
                auto var = lval.sym();
                assert(var->vclass() == sLOCAL || var->vclass() == sARGUMENT);

                __ emit(OP_LOAD_S, VarSlot(var->addr()));
                __ emit(OP_SWAP);
                if (lval.type()->inner()->isInt64())
                    __ emit(OP_STOR_I_I64);
                else
                    __ emit(OP_STOR_I_I32);
                break;
            }
            [[fallthrough]];
        }
        default: {
            auto var = lval.sym();
            if (var->vclass() == sLOCAL || var->vclass() == sARGUMENT) {
                if (var->type()->isInt64() && var->vclass() == sARGUMENT) {
                    __ emit(OP_LOAD_S, VarSlot(var->addr()));
                    __ emit(OP_SWAP);
                    __ emit(OP_STOR_I_I64);
                } else {
                    __ emit(OP_STOR_S, VarSlot(var->addr()));
                }
            } else {
                uint16_t slot = AcquireGlobalSlot(var);
                __ emit(OP_STOR_GLB, VarSlot(slot));
            }
            break;
        }
    }
}

void CodeGenerator::EmitAddress(const value& lval) {
    switch (lval.ident) {
        case iVARIABLE:
            EmitAddress(lval.sym());
            break;
        case iFIELD: {
            auto field = lval.field();
            uint32_t ref = rtti_->AddFieldRef(field);
            __ emit(OP_ADDR_FLD, ref);
            break;
        }
        case iARRAYELEM:
            if (!lval.type()->isArray())
                __ emit(OP_IDXADDR);
            break;
        default:
            assert(false);
            break;
    }
}

void CodeGenerator::EmitAddress(VarDeclBase* decl) {
    if (decl->vclass() == sARGUMENT) {
        if (decl->type()->isPassByRef())
            __ emit(OP_LOAD_S, VarSlot(decl->addr()));
        else
            __ emit(OP_ADDR_S, VarSlot(decl->addr()));
    } else if (decl->vclass() == sLOCAL) {
        if (decl->type()->isAddressType())
            __ emit(OP_LOAD_S, VarSlot(decl->addr()));
        else
            __ emit(OP_ADDR_S, VarSlot(decl->addr()));
    } else {
        assert(decl->vclass() == sSTATIC || decl->vclass() == sGLOBAL);
        uint16_t slot = AcquireGlobalSlot(decl);
        if (decl->type()->isAddressType())
            __ emit(OP_LOAD_GLB, VarSlot(slot));
        else
            __ emit(OP_ADDR_GLB, VarSlot(slot));
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
        __ emit(OP_ADDR_S, VarSlot(*hidden_slot));
        nargs++;
    }

    EmitCall(prop->getter(), nargs);

    if (hidden_slot)
        __ emit(OP_LOAD_S, VarSlot(*hidden_slot));
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

    // Note: we use map for ordering so the case table is sorted.
    std::map<cell, Label> case_labels;

    for (const auto& case_entry : stmt->cases()) {
        for (const auto& expr : case_entry.first) {
            const auto& v = expr->val();
            assert(v.ident == iCONSTEXPR);
            case_labels.emplace(v.constval(), Label());
        }
    }

    Label default_label;
    Label* defcase = &exit_label;
    if (stmt->default_case())
        defcase = &default_label;

    __ emit(OP_SWITCH);
    __ casetbl((int)case_labels.size(), defcase);

    for (auto& pair : case_labels)
        __ casetbl_entry(pair.first, &pair.second);

    for (const auto& case_entry : stmt->cases()) {
        Stmt* stmt_node = case_entry.second;

        for (const auto& expr : case_entry.first) {
            const auto& v = expr->val();
            __ bind(&case_labels[v.constval()]);
        }

        EmitStmt(stmt_node);
        if (stmt_node->flow_type() == Flow_None)
            __ emit(OP_JUMP, &exit_label);
    }

    if (stmt->default_case()) {
        __ bind(&default_label);

        EmitStmt(stmt->default_case());
        if (stmt->default_case()->flow_type() == Flow_None)
            __ emit(OP_JUMP, &exit_label);
    }

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
    ret_2d_array_ = {};

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

    if (ret_2d_array_.used()) {
        __ bind(&ret_2d_array_);
        std::vector<uint32_t> slots;
        Emit2dArrayCopy(fun_->return_type()->as<ArrayType>(), slots);
        __ emit(OP_RETV);
    }

    if (info->body()->flow_type() != Flow_Return) {
        // MustReturnValue can be false, even for non-void functions. This
        // preserves compatibility with legacy scripts where "public" allowed
        // implicit "return 0".
        if (info->MustReturnValue() || !info->return_type()->isVoid()) {
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

void CodeGenerator::Emit2dArrayCopy(ArrayType* type, std::vector<uint32_t>& slots) {
    assert(type->size());
    assert(type->inner()->isArray());

    uint32_t iter_slot = AcquireTempSlot(fun_, BuiltinType::Int);
    __ emit(OP_STOR_S_C, VarSlot(iter_slot), 0);

    slots.emplace_back(iter_slot);

    Label done, cont;
    __ bind(&cont);
    __ emit(OP_DUP);
    __ emit(OP_LOAD_S, VarSlot(iter_slot));
    __ emit(OP_IDXADDR);
    __ emit(OP_LOAD_I_I32);

    auto inner = type->inner()->as<ArrayType>();
    if (inner->inner()->isArray()) {
        Emit2dArrayCopy(inner, slots);
    } else {
        __ load_hidden_arg(fun_);
        for (const auto& slot : slots) {
            __ emit(OP_LOAD_S, VarSlot(slot));
            __ emit(OP_IDXADDR);
            __ emit(OP_LOAD_I_I32);
        }
        __ emit(OP_SWAP);
        __ emit(OP_COPYARRAY);
    }

    __ emit(OP_LOAD_S, VarSlot(iter_slot));
    __ emit(OP_INC);
    __ emit(OP_DUP);
    __ PUSH_C(type->size());
    __ emit(OP_JSGEQ, &done);
    __ emit(OP_STOR_S, VarSlot(iter_slot));
    __ emit(OP_JUMP, &cont);
    __ bind(&done);

    slots.pop_back();

    // Caller pushed a value.
    __ emit(OP_POP);
}

void CodeGenerator::EmitEnumStructDecl(EnumStructDecl* decl) {
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

void CodeGenerator::EmitNumber64Expr(Number64Expr* expr) {
    __ emit(OP_PUSH_C_I64, Int64Value(*expr->ToInt64()));
}

void CodeGenerator::EmitSimpleCastExpr(SimpleCastExpr* expr) {
    EmitExpr(expr->from());

    Type* from_type = expr->from()->val().type();

    if (expr->to()->isInt64()) {
        assert(from_type->isInt() || from_type->isAny());
        __ emit(OP_CVT_I64);
    } else if (expr->to()->isBool()) {
        if (from_type->isInt64())
            __ emit(OP_TEST);
        else
            assert(false);
    } else {
        __ emit(OP_CVT_F32);
    }
}

static inline bool CoercesToInt64(Type* type) {
    return type->isInt() || type->isAny();
}

void CodeGenerator::EmitCastExpr(CastExpr* expr, unsigned int flags) {
    auto from = expr->expr();
    if (expr->lvalue()) {
        assert(from->lvalue());

        auto val = BindLvalue(from);
        EmitAddress(val);
    } else {
        EmitExpr(from);

        if (CoercesToInt64(expr->val().type()) && from->val().type()->isInt64()) {
            __ emit(OP_TRUNCATE_I64);
        } else if (expr->val().type()->isInt64() && CoercesToInt64(from->val().type())) {
            __ emit(OP_CVT_I64);
        }
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
    return AcquireTempSlot(node, cc_.types()->GetBuiltin(builtin_type));
}

cell_t CodeGenerator::AcquireTempSlot(ParseNode* node, Type* type) {
    auto iter = free_temp_slots_.begin();
    while (iter != free_temp_slots_.end()) {
        if ((*iter).second == type) {
            used_temp_slots_.splice(used_temp_slots_.end(), free_temp_slots_, iter);
            return (*iter).first;
        }
        iter++;
    }

    uint32_t slot = rtti_->AddLocalSlot(&locals_, QualType(type));
    if (slot > INT16_MAX)
        report(node->pos(), 467);
    used_temp_slots_.emplace_back(slot, type);
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
