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
#include <amtl/am-unused.h>

#include "array-helpers.h"
#include "assembler.h"
#include "code-generator.h"
#include "compile-context.h"
#include "compile-options.h"
#include "errors.h"
#include "expressions.h"
#include "sctracker.h"
#include "symbols.h"
#include "value-inl.h"

namespace sp {
namespace cc {

#define __ asm_.

CodeGenerator::CodeGenerator(CompileContext& cc, std::shared_ptr<ir::Module> mod)
  : cc_(cc),
    mod_(mod)
{
}

bool CodeGenerator::Generate() {
    // First instruction is always halt.
    __ emit(OP_HALT, 0);

    for (const auto& var : mod_->globals())
        EmitGlobalVar(var);

    for (const auto& fun : mod_->functions())
        EmitFunction(fun);

    if (!ComputeStackUsage())
        return false;

    // Finish any un-added debug symbols.
    while (!static_syms_.empty()) {
        auto pair = ke::PopBack(&static_syms_);
        AddDebugSymbols(&pair.second);
    }

    AddDebugSymbols(&global_syms_);

    return errors_.ok();
}

void
CodeGenerator::AddDebugFile(const std::string& file)
{
    auto str = ke::StringPrintf("F:%x %s", asm_.position(), file.c_str());
    debug_strings_.emplace_back(str.c_str(), str.size());
}

void
CodeGenerator::AddDebugLine(int linenr)
{
#if 0
    auto str = ke::StringPrintf("L:%x %x", asm_.position(), linenr);
    if (fun_) {
        auto data = fun_->cg();
        if (!data->dbgstrs)
            data->dbgstrs = cc_.NewDebugStringList();
        data->dbgstrs->emplace_back(str.c_str(), str.size());
    } else {
        debug_strings_.emplace_back(str.c_str(), str.size());
    }
#endif
}

void CodeGenerator::AddDebugSymbol(Decl* decl, uint32_t pc) {
    auto symname = decl->name()->chars();

    std::optional<cell> addr;
    if (auto fun = decl->as<FunctionDecl>()) {
        addr.emplace(fun->cg()->label.offset());
    } else if (auto var = decl->as<VarDeclBase>()) {
        if (auto cv = var->as<ConstDecl>())
            addr.emplace(cv->const_val());
        else
            addr.emplace(var->addr());
    }

    /* address tag:name codestart codeend ident vclass [tag:dim ...] */
    auto string = ke::StringPrintf("S:%x %x:%s %x %x %x %x %x",
                                   *addr, decl->type()->type_index(), symname, pc,
                                   asm_.position(), decl->ident(), decl->vclass(), (int)decl->is_const());

    if (fun_) {
#if 0
        auto data = fun_->cg();
        if (!data->dbgstrs)
            data->dbgstrs = cc_.NewDebugStringList();
        data->dbgstrs->emplace_back(string.c_str(), string.size());
#endif
    } else {
        debug_strings_.emplace_back(string.c_str(), string.size());
    }
}

void CodeGenerator::AddDebugSymbols(tr::vector<DebugSymbol>* list) {
    while (!list->empty()) {
        auto entry = ke::PopBack(list);
        AddDebugSymbol(entry.first, entry.second);
    }
}

void CodeGenerator::EmitBlock(ir::InsnBlock* block) {
    if (block->has_heap_allocs())
        EnterHeapScope(Flow_None);

    for (auto iter = block->list(); iter; iter = iter->next())
        EmitInsn(iter);

    if (block->has_heap_allocs())
        LeaveHeapScope();
}

void CodeGenerator::EmitInsn(ir::Insn* node) {
    switch (node->kind()) {
        case IrKind::Return:
            EmitReturn(node->to<ir::Return>());
            break;
        case IrKind::Variable:
            EmitVarDecl(node->to<ir::Variable>());
            break;
        case IrKind::ValueInsn:
            EmitValueInsn(node->to<ir::ValueInsn>());
            break;
        case IrKind::DoWhile:
            EmitDoWhile(node->to<ir::DoWhile>());
            break;
        case IrKind::If:
            EmitIf(node->to<ir::If>());
            break;
        case IrKind::Break:
            EmitLoopControl(tBREAK);
            break;
        case IrKind::Continue:
            EmitLoopControl(tCONTINUE);
            break;
        case IrKind::Switch:
            EmitSwitch(node->to<ir::Switch>());
            break;
        case IrKind::ForLoop:
            EmitForLoop(node->to<ir::ForLoop>());
            break;
        case IrKind::Delete:
            EmitDelete(node->to<ir::Delete>());
            break;
        default:
            assert(false);
    }
}

void CodeGenerator::EmitValueInsn(ir::ValueInsn* insn) {
    EmitValue(insn->val());
}

void CodeGenerator::EmitCommaOp(ir::CommaOp* op) {
    for (const auto& val : op->values())
        EmitValue(val);
}

#if 0
void
CodeGenerator::EmitStmt(Stmt* stmt)
{
    if (fun_) {
        AddDebugLine(stmt->pos().line);
        EmitBreak();
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
            pushstacklist();

            {
                AutoEnterScope locals(this, &local_syms_);
                EmitStmtList(s);
            }

            bool returns = s->flow_type() == Flow_Return;
            popstacklist(!returns);
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
}
#endif

void
CodeGenerator::EmitChangeScopeNode(ChangeScopeNode* node)
{
    AddDebugFile(node->file()->chars());
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

void CodeGenerator::EmitVarDecl(ir::Variable* var) {
    if (var->decl()->vclass() == sLOCAL)
        EmitLocalVar(var);
    else
        EmitGlobalVar(var);
#if 0
    if (decl->is_public() || decl->is_used())
        EnqueueDebugSymbol(decl, asm_.position());
#endif
}

void CodeGenerator::EmitGlobalVar(ir::Variable* var) {
    auto decl = var->decl();
    auto init = var->init();

    var->set_addr(data_.dat_address());

    if (decl->type()->isArray() || (decl->ident() == iVARIABLE && decl->type()->isEnumStruct())) {
        ArrayData array;
        BuildCompoundInitializer(QualType(decl->type()), var->init(), &array, data_.dat_address());

        data_.Add(std::move(array.iv));
        data_.Add(std::move(array.data));
        data_.AddZeroes(array.zeroes);
    } else if (decl->ident() == iVARIABLE) {
        cell_t cells = 1;
        if (auto es = decl->type()->asEnumStruct())
            cells = es->array_size();

        if (init) {
            auto cv = init->to<ir::Const>();
            data_.Add(cv->value());
        } else {
            data_.AddZeroes(cells);
        }
    } else {
        assert(false);
    }
}

void CodeGenerator::EmitLocalVar(ir::Variable* var) {
    auto init = var->init();
    bool is_struct = var->decl()->type()->isEnumStruct();
    bool is_array = var->decl()->type()->isArray();

    if (!is_array && !is_struct) {
        cell_t addr = markstack(var->pn(), MEMUSE_STATIC, 1);
        var->set_addr(addr);

        if (init) {
            if (auto cv = init->as<ir::Const>()) {
                __ emit(OP_PUSH_C, cv->value());
            } else {
                EmitValue(init);
                __ emit(OP_PUSH_PRI);
            }
        } else {
            // Note: we no longer honor "decl" for scalars.
            __ emit(OP_PUSH_C, 0);
        }
    } else {
        cell_t addr = markstack(var->decl(), MEMUSE_STATIC, 1);
        var->set_addr(addr);

        if (!init || init->as<ir::ArrayInitializer>()) {
            ArrayData array;
            BuildCompoundInitializer(QualType(var->decl()->type()), var->init(), &array);

            cell iv_size = (cell)array.iv.size();
            cell data_size = (cell)array.data.size() + array.zeroes;
            cell total_size = iv_size + data_size;

            TrackHeapAlloc(var->decl(), MEMUSE_STATIC, total_size);

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
            if (!var->decl()->autozero() && array.zeroes)
                array.zeroes = 0;

            __ emit(OP_HEAP, total_size * sizeof(cell));
            __ emit(OP_INITARRAY_ALT, iv_addr, iv_size, non_filled, array.zeroes, 0);
            __ emit(OP_PUSH_ALT);
        } else if (auto ctor = init->as<ir::NewArray>()) {
            TrackHeapAlloc(var->decl(), MEMUSE_DYNAMIC, 0);
            for (const auto& dim : ctor->dims()) {
                EmitValue(dim);
                __ emit(OP_PUSH_PRI);
            }
            if (var->decl()->autozero())
                __ emit(OP_GENARRAY_Z, ctor->dims().size());
            else
                __ emit(OP_GENARRAY, ctor->dims().size());
        } else if (auto ctor = init->as<ir::CharArrayLiteral>()) {
            auto queue_size = data_.size();
            auto str_addr = data_.dat_address();
            data_.Add(ctor->expr()->text()->chars(), ctor->expr()->text()->length() + 1);

            auto cells = data_.size() - queue_size;
            assert(cells > 0);

            TrackHeapAlloc(var->decl(), MEMUSE_DYNAMIC, cells);

            __ emit(OP_PUSH_C, cells);
            if (var->decl()->autozero())
                __ emit(OP_GENARRAY_Z, 1);
            else
                __ emit(OP_GENARRAY, 1);
            __ const_pri(str_addr);
            __ copyarray(var, cells * sizeof(cell));
        } else {
            assert(false);
        }
    }
}

#if 0
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
#endif

void CodeGenerator::EmitValue(ir::Value* val) {
    AutoErrorPos aep(val->pn()->pos());

    // l-values must be either handled by stores or loads.
    assert(!val->as<ir::Lvalue>());

    switch (val->kind()) {
        case IrKind::ConstVal:
            EmitConst(val->to<ir::Const>());
            break;
        case IrKind::UnaryOp:
            EmitUnary(val->to<ir::UnaryOp>());
            break;
        case IrKind::Load:
            EmitLoad(val->to<ir::Load>());
            break;
        case IrKind::CommaOp:
            EmitCommaOp(val->to<ir::CommaOp>());
            break;
        case IrKind::CallOp:
            EmitCallOp(val->to<ir::CallOp>());
            break;
        case IrKind::BinaryOp:
            EmitBinary(val->to<ir::BinaryOp>());
            break;
        case IrKind::CharArrayLiteral:
            EmitCharArrayLiteral(val->to<ir::CharArrayLiteral>());
            break;
        case IrKind::TernaryOp:
            EmitTernaryOp(val->to<ir::TernaryOp>());
            break;
        case IrKind::Store:
            EmitStore(val->to<ir::Store>());
            break;
        case IrKind::StoreWithTemp:
            EmitStoreWithTemp(val->to<ir::StoreWithTemp>());
            break;
        case IrKind::FunctionRef:
            EmitFunctionRef(val->to<ir::FunctionRef>());
            break;
        case IrKind::AddressOf:
            EmitAddressOf(val->to<ir::AddressOf>());
            break;
        case IrKind::CallUserOp:
            EmitCallUserOp(val->to<ir::CallUserOp>());
            break;
        case IrKind::IncDec:
        case IrKind::IncDecUserOp:
            EmitIncDec(val->to<ir::IncDec>());
            break;
        case IrKind::TempValueRef:
            EmitTempValueRef(val->to<ir::TempValueRef>());
            break;
        case IrKind::CastOp: {
            auto inner = val->to<ir::CastOp>()->val();
            EmitValue(inner);
            break;
        }
        case IrKind::ArrayInitializer: {
            EmitArrayInitializer(val->to<ir::ArrayInitializer>());
            break;
        }
        default:
            assert(false);
            break;
    }
}

void CodeGenerator::EmitLoad(ir::Load* load) {
    auto lval = load->lval();
    EmitLoadStorePrologue(lval);
    EmitLoadEpilogue(lval);
}

void CodeGenerator::EmitAddressOf(ir::AddressOf* op) {
    auto lval = op->lval();
    switch (lval->kind()) {
        case IrKind::VariableRef: {
            auto ref = lval->as<ir::VariableRef>();
            auto var = ref->var();
            auto vclass = var->decl()->vclass();
            if (vclass == sSTATIC || vclass == sGLOBAL)
                __ const_pri(var->addr());
            else if (ref->type()->isReference())
                __ emit(OP_LOAD_S_PRI, var->addr());
            else
                __ emit(OP_ADDR_PRI, var->addr());
            break;
        }
        case IrKind::IndexOp: {
            auto index = lval->to<ir::IndexOp>();
            EmitAddressOfIndexOp(index);

            if (index->type()->isArray())
                __ emit(OP_LOAD_I);
            break;
        }
        case IrKind::StackRef: {
            auto op = lval->to<ir::StackRef>();
            __ emit(OP_ADDR_PRI, GetSlotAddr(op->slot()));
            break;
        }
        case IrKind::FieldRef: {
            auto op = lval->to<ir::FieldRef>();
            EmitAddressOfFieldRef(op);
            break;
        }
        default:
            assert(false);
    }
}

void CodeGenerator::EmitTempValueRef(ir::TempValueRef* ref) {
    uint32_t slot_addr = GetSlotAddr(ref->slot());
    EmitValue(ref->val());
    __ emit(OP_STOR_S_PRI, slot_addr);
    __ emit(OP_ADDR_PRI, slot_addr);
}

void CodeGenerator::EmitStore(ir::Store* op) {
    auto lval = op->lval();
    bool save_pri = EmitLoadStorePrologue(lval);
    if (save_pri)
        __ emit(OP_PUSH_PRI);

    EmitValue(op->val());

    if (save_pri)
        __ emit(OP_POP_ALT);

    EmitStoreEpilogue(lval, true /* save_rval */);
}

void CodeGenerator::EmitStoreWithTemp(ir::StoreWithTemp* op) {
    auto lval = op->lval();
    cell_t temp_addr = GetSlotAddr(op->temp_slot());

    [[maybe_unused]] bool save_pri = EmitLoadStorePrologue(lval);
    assert(save_pri);

    // Save the lval address, then dereference it, and store the value in the
    // temporary stack location. This will get used by the Load(StackRef) in
    // the right-hand side of the store.
    __ emit(OP_PUSH_PRI);
    EmitLoadEpilogue(lval);
    __ emit(OP_STOR_S_PRI, temp_addr);
    EmitValue(op->val());
    __ emit(OP_POP_ALT);
    EmitStoreEpilogue(lval, true /* save_rval */);
}

void CodeGenerator::EmitIncDec(ir::IncDec* incdec) {
    auto lval = incdec->lval();
    bool prefix = incdec->expr()->prefix();
    bool used = incdec->used();

    // If the postfix value isn't used, it can be CG'd as a prefix operation.
    bool postfix = used && !prefix;

    bool save_pri = EmitLoadStorePrologue(lval);
    if (save_pri)
        __ emit(OP_PUSH_PRI);
    EmitLoadEpilogue(lval);
    if (auto user_op = incdec->as<ir::IncDecUserOp>()) {
        if (postfix) {
            if (save_pri) {
                // Re-order stack so that the saved address is popped first.
                // Note, SWAP doesn't quite do what we want here.
                __ emit(OP_POP_ALT);
                __ emit(OP_PUSH_PRI);
                __ emit(OP_PUSH_ALT);
            } else {
                __ emit(OP_PUSH_PRI);
            }
        }
        // Push the value to increment.
        __ emit(OP_PUSH_PRI);
        EmitCall(user_op->target(), 1);
        if (save_pri)
            __ emit(OP_POP_ALT);
    } else {
        if (save_pri)
            __ emit(OP_POP_ALT);
        if (postfix)
            __ emit(OP_PUSH_PRI);
        if (incdec->expr()->token() == tINC)
            __ emit(OP_ADD_C, 1);
        else
            __ emit(OP_ADD_C, -1);
    }
    // Calling setters might return a bogus PRI. Make sure to save it if we
    // need the prefix value.
    EmitStoreEpilogue(lval, prefix && used /* save_rval */);
    if (postfix)
        __ emit(OP_POP_PRI);
}

void CodeGenerator::EmitCallOp(ir::CallOp* call) {
    auto target = call->target();
    if (target->fun()->return_array() && !target->fun()->decl()->is_native()) {
        cell_t retsize = target->fun()->decl()->return_type()->CellStorageSize();
        assert(retsize);

        __ emit(OP_HEAP, retsize * sizeof(cell));
        __ emit(OP_PUSH_ALT);
        TrackTempHeapAlloc(call->expr(), retsize * sizeof(cell));
    }

    const auto& argv = call->argv();
    cell_t nargs = 0;
    for (size_t i = argv.size() - 1; i < argv.size(); i--) {
        EmitArg(argv[i]);
        nargs++;
    }

    EmitCall(target->fun(), nargs);

    if (target->fun()->return_array() && !target->fun()->decl()->is_native())
        __ emit(OP_POP_PRI);
}

void CodeGenerator::EmitArg(ir::Value* val) {
    if (auto init = val->as<ir::ArrayInitializer>()) {
        ArrayData array;
        BuildCompoundInitializer(QualType(init->type()), init, &array);

        __ emit(OP_HEAP, array.total_size() * sizeof(cell));
        __ emit(OP_INITARRAY_ALT, data_.dat_address(), (cell_t)array.iv.size(),
                (cell_t)array.data.size(), (cell_t)array.zeroes, 0);
        __ emit(OP_MOVE_PRI);

        data_.Add(std::move(array.iv));
        data_.Add(std::move(array.data));
    } else if (auto slice = val->as<ir::SliceArray>()) {
        EmitAddressOfIndexOp(slice->index_op());
    } else {
        EmitValue(val);
    }
    __ emit(OP_PUSH_PRI);
}

void CodeGenerator::EmitArrayInitializer(ir::ArrayInitializer* array) {
    ArrayData array_data;
    BuildCompoundInitializer(array->type(), array, &array_data);

    TrackHeapAlloc(array->expr(), MEMUSE_STATIC, array_data.total_size());

    __ emit(OP_HEAP, array_data.total_size() * sizeof(cell_t));
    __ emit(OP_INITARRAY_ALT, data_.dat_address(), (cell_t)array_data.iv.size(),
            (cell_t)array_data.data.size(), (cell_t)array_data.zeroes, 0);
    __ emit(OP_MOVE_PRI);

    data_.Add(std::move(array_data.iv));
    data_.Add(std::move(array_data.data));
}

void CodeGenerator::EmitFunctionRef(ir::FunctionRef* ref) {
    __ emit(OP_CONST_PRI, &ref->fun()->public_id());
}

void CodeGenerator::EmitConst(ir::Const* cv) {
    __ const_pri(cv->value());
}

static inline ir::BinaryOp* ToLogical(ir::Value* op) {
    if (auto bin = op->as<ir::BinaryOp>()) {
        if (bin->token() == tlAND || bin->token() == tlOR)
            return bin;
    }
    return nullptr;
}

void CodeGenerator::EmitTest(ir::Value* test, bool jump_on_true, sp::Label* target) {
    switch (test->kind()) {
        case IrKind::UnaryOp:
            if (EmitUnaryExprTest(test->to<ir::UnaryOp>(), jump_on_true, target))
                return;
            break;
        case IrKind::BinaryOp:
            if (auto bin = ToLogical(test)) {
                EmitLogicalExprTest(bin->to<ir::BinaryOp>(), jump_on_true, target);
                return;
            }
            break;
#if 0
        case ExprKind::ChainedCompareExpr:
            if (EmitChainedCompareExprTest(expr->to<ChainedCompareExpr>(), jump_on_true, target))
                return;
            break;
#endif
    }

    EmitValue(test);

    if (jump_on_true)
        __ emit(OP_JNZ, target);
    else
        __ emit(OP_JZER, target);
}

void CodeGenerator::EmitUnary(ir::UnaryOp* op) {
    EmitValue(op->val());

    switch (op->expr()->token()) {
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

bool CodeGenerator::EmitUnaryExprTest(ir::UnaryOp* op, bool jump_on_true, Label* target) {
    if (op->expr()->token() == '!') {
        EmitTest(op->val(), !jump_on_true, target);
        return true;
    }
    return false;
}

void CodeGenerator::EmitBinary(ir::BinaryOp* op) {
    if (op->token() == tlOR || op->token() == tlAND) {
        bool jump_on_true = (op->token() == tlOR);

        Label shortcircuit, done;

        EmitTest(op, jump_on_true, &shortcircuit);
        __ const_pri(!jump_on_true);
        __ emit(OP_JUMP, &done);
        __ bind(&shortcircuit);
        __ const_pri(jump_on_true);
        __ bind(&done);
        return;
    }

    if (auto left_cv = op->left()->as<ir::Const>()) {
        if (auto right_cv = op->right()->as<ir::Const>())
            __ const_pri(right_cv->value());
        else
            EmitValue(op->right());
        __ const_alt(left_cv->value());
    } else {
        EmitValue(op->left());

        if (auto right_cv = op->right()->as<ir::Const>()) {
            if (IsOperTokenCommutative(op->token())) {
                __ const_alt(right_cv->value());
            } else {
                __ emit(OP_PUSH_PRI);
                __ const_pri(right_cv->value());
                __ emit(OP_POP_ALT);
            }
        } else {
            __ emit(OP_PUSH_PRI);
            EmitValue(op->right());
            __ emit(OP_POP_ALT);
        }
    }

    switch (op->token()) {
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

void CodeGenerator::EmitTernaryOp(ir::TernaryOp* op) {
    Label on_false, done;
    EmitTest(op->select(), false, &on_false);
    EmitValue(op->on_true());
    __ emit(OP_JUMP, &done);
    __ bind(&on_false);
    EmitValue(op->on_false());
    __ bind(&done);
}

void CodeGenerator::EmitCharArrayLiteral(ir::CharArrayLiteral* val) {
    auto text = val->expr()->text();
    auto addr = data_.dat_address();
    data_.Add(text->chars(), text->length());
    __ const_pri(addr);
}

bool CodeGenerator::EmitLoadStorePrologue(ir::Lvalue* lval) {
    switch (lval->kind()) {
        case IrKind::VariableRef:
            return false;

        case IrKind::IndexOp:
            EmitAddressOfIndexOp(lval->to<ir::IndexOp>());
            return true;

        // StackRef can have a convenience initializer.
        case IrKind::StackRef:
            return false;

        case IrKind::FieldRef:
            EmitAddressOfFieldRef(lval->to<ir::FieldRef>());
            return true;

        case IrKind::PropertyRef: {
            auto ref = lval->to<ir::PropertyRef>();
            EmitValue(ref->val());
            return true;
        }

        default:
            assert(false);
            return true;
    }
}

void CodeGenerator::EmitLoadEpilogue(ir::Lvalue* lval) {
    switch (lval->kind()) {
        case IrKind::VariableRef:
            EmitLoadVariable(lval->to<ir::VariableRef>());
            break;
        case IrKind::IndexOp: {
            auto op = lval->to<ir::IndexOp>();
            if (op->base()->type()->isCharArray())
                __ emit(OP_LODB_I, 1);
            else if (!op->type()->isEnumStruct())
                __ emit(OP_LOAD_I);
            break;
        }
        case IrKind::StackRef: {
            auto ref = lval->to<ir::StackRef>();
            __ emit(OP_LOAD_S_PRI, GetSlotAddr(ref->slot()));
            break;
        }
        case IrKind::FieldRef: {
            auto op = lval->to<ir::FieldRef>();
            if (!(op->type()->isArray() || op->type()->isEnumStruct()))
                __ emit(OP_LOAD_I);
            break;
        }
        case IrKind::PropertyRef: {
            auto ref = lval->to<ir::PropertyRef>();
            assert(ref->getter());
            __ emit(OP_PUSH_PRI);
            EmitCall(ref->getter(), 1);
            break;
        }
        default:
            assert(false);
    }
}

void CodeGenerator::EmitLoadVariable(ir::VariableRef* ref) {
    auto var = ref->var();
    auto vclass = var->decl()->vclass();
    if (vclass == sLOCAL || vclass == sARGUMENT) {
        if (ref->type()->isReference())
            __ emit(OP_LREF_S_PRI, var->addr());
        else
            __ emit(OP_LOAD_S_PRI, var->addr());
    } else if (ref->type()->isArray() || ref->type()->isEnumStruct()) {
        __ emit(OP_CONST_PRI, var->addr());
    } else {
        __ emit(OP_LOAD_PRI, var->addr());
    }
}

void CodeGenerator::EmitAddressOfIndexOp(ir::IndexOp* op) {
    EmitValue(op->base());

    cell_t rank_size = sizeof(cell_t);

    auto array_type = op->base()->type()->as<ArrayType>();
    if (!array_type->inner()->isArray()) {
        if (array_type->inner()->isChar())
            rank_size = 1;
        else if (auto es = array_type->inner()->asEnumStruct())
            rank_size = es->array_size() * sizeof(cell_t);
    }

    assert(rank_size == 1 || (rank_size % sizeof(cell_t) == 0));

    if (auto cv = op->index()->as<ir::Const>()) {
        if (cv->value() != 0)
            __ emit(OP_ADD_C, cv->value() * rank_size);
    } else {
        __ emit(OP_PUSH_PRI);
        EmitValue(op->index());

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
}

void CodeGenerator::EmitAddressOfFieldRef(ir::FieldRef* ref) {
    EmitValue(ref->base());

    if (ref->field()->offset()) {
        __ const_alt(ref->field()->offset() * sizeof(cell_t));
        __ emit(OP_ADD);
    }
}


void CodeGenerator::EmitStoreEpilogue(ir::Lvalue* lval, bool save_rval) {
    switch (lval->kind()) {
        case IrKind::VariableRef: {
            auto ref = lval->to<ir::VariableRef>();
            auto var = ref->var();
            auto decl = var->decl();
            if (decl->vclass() == sSTATIC || decl->vclass() == sGLOBAL) {
                __ emit(OP_STOR_PRI, var->addr());
            } else if (decl->type()->isReference()) {
                __ emit(OP_SREF_S_PRI, var->addr());
            } else {
                __ emit(OP_STOR_S_PRI, var->addr());
            }
            break;
        }

        case IrKind::IndexOp: {
            auto op = lval->to<ir::IndexOp>();
            if (op->base()->type()->isCharArray())
                __ emit(OP_STRB_I, 1);
            else
                __ emit(OP_STOR_I);
            break;
        }

        case IrKind::FieldRef:
            __ emit(OP_STOR_I);
            break;

        case IrKind::PropertyRef: {
            auto ref = lval->to<ir::PropertyRef>();
            assert(ref->setter());

            if (save_rval)
                __ emit(OP_PUSH_PRI);
            __ emit(OP_PUSH_PRI);
            __ emit(OP_PUSH_ALT);
            EmitCall(ref->setter(), 2);
            if (save_rval)
                __ emit(OP_POP_PRI);
            break;
        }

        default:
            assert(false);
    }
}

static void FlattenLogical(ir::Value* op, int token, std::vector<ir::Value*>* out) {
    if (auto bin = ToLogical(op)) {
        if (bin->token() == token) {
            out->emplace_back(bin->left());
            out->emplace_back(bin->right());
            return;
        }
    }
    out->emplace_back(op);
}

void CodeGenerator::EmitLogicalExprTest(ir::BinaryOp* root, bool jump_on_true, sp::Label* target) {
    assert(root->token() == tlAND || root->token() == tlOR);

    std::vector<ir::Value*> sequence;
    FlattenLogical(root->left(), root->token(), &sequence);
    FlattenLogical(root->right(), root->token(), &sequence);

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
        auto op = sequence.at(i);
        if (root->token() == tlOR) {
            if (jump_on_true)
                EmitTest(op, true, target);
            else
                EmitTest(op, true, &fallthrough);
        } else {
            assert(root->token() == tlAND);
            if (jump_on_true)
                EmitTest(op, false, &fallthrough);
            else
                EmitTest(op, false, target);
        }
    }

    EmitTest(sequence.back(), jump_on_true, target);
    __ bind(&fallthrough);
}

#if 0
bool
CodeGenerator::EmitChainedCompareExprTest(ChainedCompareExpr* root, bool jump_on_true,
                                          Label* target)
{
    // No optimization for user operators or for compare chains.
    if (root->ops().size() > 1 || root->ops()[0].userop.sym)
        return false;

    Expr* left = root->first();
    Expr* right = root->ops()[0].expr;

    EmitExpr(left);
    __ emit(OP_PUSH_PRI);
    EmitExpr(right);
    __ emit(OP_POP_ALT);
    __ emit(OP_XCHG);

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
#endif

#if 0
void CodeGenerator::EmitCallExpr(CallExpr* call) {
    // If returning an array, push a hidden parameter.
    if (call->fun()->return_array()) {
        cell retsize = call->fun()->return_type()->CellStorageSize();
        assert(retsize);

        __ emit(OP_HEAP, retsize * sizeof(cell));
        __ emit(OP_PUSH_ALT);
        TrackTempHeapAlloc(call, 1);
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
                    __ setheap_pri();
                    TrackTempHeapAlloc(expr, 1);
                } else if (lvalue) {
                    __ address(val.sym, sPRI);
                } else {
                    __ setheap_pri();
                    TrackTempHeapAlloc(expr, 1);
                }
            } else if (val.ident == iCONSTEXPR || val.ident == iEXPRESSION) {
                /* allocate a cell on the heap and store the
                 * value (already in PRI) there */
                __ setheap_pri();
                TrackTempHeapAlloc(expr, 1);
            }
        } else {
            if (arg->type_info().type->isReference()) {
                if (val.ident == iVARIABLE) {
                    assert(val.sym);
                    __ address(val.sym, sPRI);
                }
            }
        }

        __ emit(OP_PUSH_PRI);
    }

    EmitCall(call->fun(), (cell)argv.size());

    if (call->fun()->return_array())
        __ emit(OP_POP_PRI);
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
#endif

void CodeGenerator::EmitCallUserOp(ir::CallUserOp* op) {
    auto opertok = op->target()->decl()->decl().opertok;

    switch (opertok) {
        case tINC:
        case tDEC:
            assert(false);
            break;
        case tlLE:
        case tlGE:
        case '>':
        case '<':
            break;
        case '=':
            assert(false);
            break;
        case '~':
        case '!':
            break;
        case '+':
        case '-':
        case '*':
        case '/':
        case '%':
        case tlNE:
        case tlEQ:
            break;
        default:
            assert(false);
    }

    EmitValue(op->first());
    __ emit(OP_PUSH_PRI);

    if (op->second()) {
        assert(op->target()->argv().size() == 2);

        // Arguments are pushed in reverse order.
        EmitValue(op->second());
        if (op->swapped()) {
            __ emit(OP_PUSH_PRI);
        } else {
            // PRI = right, stack = [left]. Swap them.
            __ emit(OP_SWAP_PRI);
            // PRI = left, stack = [right]. Push left val.
            __ emit(OP_PUSH_PRI);
        }
        EmitCall(op->target(), 2);
    } else {
        EmitCall(op->target(), 1);
    }
}

#if 0
void CodeGenerator::EmitNewArrayExpr(NewArrayExpr* expr) {
    int numdim = 0;
    auto& exprs = expr->exprs();
    const auto& type = expr->type();
    for (size_t i = 0; i < exprs.size(); i++) {
        EmitExpr(exprs[i]);

        if (i == exprs.size() - 1 && type->isChar())
            __ emit(OP_STRADJUST_PRI);

        __ emit(OP_PUSH_PRI);
        numdim++;
    }

    auto innermost = type;
    while (innermost->isArray())
        innermost = innermost->to<ArrayType>()->inner();

    if (auto es = innermost->asEnumStruct()) {
        // The last dimension is implicit in the size of the enum struct. Note
        // that when synthesizing a NewArrayExpr for old-style declarations,
        // it is impossible to have an enum struct.
        // :TODO: test this
        __ emit(OP_PUSH_C, es->array_size());
        numdim++;
    }

    if (expr->autozero())
        __ emit(OP_GENARRAY_Z, numdim);
    else
        __ emit(OP_GENARRAY, numdim);
}
#endif

void CodeGenerator::EmitIf(ir::If* insn) {
    Label flab1;

    EmitTest(insn->cond(), false, &flab1);
    EmitBlock(insn->on_true());
    if (insn->on_false()) {
        Label flab2;
        __ emit(OP_JUMP, &flab2);
        __ bind(&flab1);
        EmitBlock(insn->on_false());
        __ bind(&flab2);
    } else {
        __ bind(&flab1);
    }
}

void CodeGenerator::EmitReturnArray(ir::Return* node) {
    ArrayData array;
    BuildCompoundInitializer(node->val()->type(), nullptr, &array);

    auto info = fun_->return_array();
    if (array.iv.empty()) {
        // A much simpler copy can be emitted.
        __ load_hidden_arg(fun_, true);

        cell size = fun_->decl()->return_type()->CellStorageSize();
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
    __ load_hidden_arg(fun_, false);
    __ emit(OP_INITARRAY_ALT, dat_addr, iv_size, 0, 0, 0);
    __ emit(OP_MOVE_PRI);
    __ emit(OP_ADD_C, iv_size * sizeof(cell));
    __ emit(OP_MOVE_ALT);
    __ emit(OP_POP_PRI);
    __ emit(OP_ADD_C, iv_size * sizeof(cell));
    __ emit(OP_MOVS, info->zeroes * sizeof(cell));
}

void CodeGenerator::EmitReturn(ir::Return* node) {
    if (node->val()) {
        EmitValue(node->val());

        if (node->val()->type()->isArray())
            EmitReturnArray(node);
    } else {
        /* this return statement contains no expression */
        __ const_pri(0);
    }

    genstackfree(-1); /* free everything on the stack */
    __ emit(OP_RETN);
}

void CodeGenerator::EmitDelete(ir::Delete* stmt) {
    auto val = stmt->val();
    auto lval = val->as<ir::Lvalue>();
    if (lval) {
        if (EmitLoadStorePrologue(lval)) {
            __ emit(OP_PUSH_PRI);
            EmitLoadEpilogue(lval);
            __ emit(OP_POP_ALT);
            // PRI = value, ALT = addr
        } else {
            EmitLoadEpilogue(lval);
            // PRI = value
        }
        // Save PRI so we can pass it as |this| to our dtor.
        __ emit(OP_PUSH_PRI);
        __ emit(OP_ZERO_PRI);
        EmitStoreEpilogue(lval, false /* save_rval */);
        __ emit(OP_POP_PRI);
    } else {
        EmitValue(val);
    }

    // push.pri
    // push.c 1
    // sysreq.c N 1
    // stack 8
    __ emit(OP_PUSH_PRI);
    EmitCall(stmt->dtor(), 1);
}

#if 0
void
CodeGenerator::EmitRvalue(value* lval)
{
    switch (lval->ident) {
        case iARRAYCELL:
            if (!lval->type()->asEnumStruct())
                __ emit(OP_LOAD_I);
            break;
        case iARRAYCHAR:
            __ emit(OP_LODB_I, 1);
            break;
        case iACCESSOR:
            InvokeGetter(lval->accessor());
            lval->ident = iEXPRESSION;
            break;
        case iVARIABLE: {
            if (lval->type()->isReference()) {
                auto var = lval->sym->as<VarDeclBase>();
                assert(var->vclass() == sLOCAL || var->vclass() == sARGUMENT);
                __ emit(OP_LREF_S_PRI, var->addr());
                break;
            }
            [[fallthrough]];
        }
        default: {
            auto var = lval->sym->as<VarDeclBase>();
            if (var->vclass() == sLOCAL || var->vclass() == sARGUMENT)
              __ emit(OP_LOAD_S_PRI, var->addr());
            else
              __ emit(OP_LOAD_PRI, var->addr());
            break;
        }
    }
}
#endif

void CodeGenerator::EmitDoWhile(ir::DoWhile* loop) {
    auto stmt = loop->stmt();
    int token = stmt->token();
    assert(token == tDO || token == tWHILE);

    LoopContext loop_cx;
    loop_cx.stack_scope_id = stack_scope_id();
    loop_cx.heap_scope_id = heap_scope_id();
    ke::SaveAndSet<LoopContext*> push_context(&loop_, &loop_cx);

    auto body = loop->body();
    auto cond = loop->cond();
    if (token == tDO) {
        Label start;
        __ bind(&start);

        EmitBlock(body);

        __ bind(&loop_cx.continue_to);
        if (0 /*cond->tree_has_heap_allocs()*/) {
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
    } else {
        __ bind(&loop_cx.continue_to);

        if (0 /*cond->tree_has_heap_allocs()*/) {
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
        EmitBlock(body);
        __ emit(OP_JUMP, &loop_cx.continue_to);
    }

    __ bind(&loop_cx.break_to);
}

void CodeGenerator::EmitLoopControl(int token) {
    assert(loop_);
    assert(token == tBREAK || token == tCONTINUE);

    genstackfree(loop_->stack_scope_id);

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

void CodeGenerator::EmitForLoop(ir::ForLoop* loop) {
    ke::Maybe<AutoEnterScope> debug_scope;

    auto stmt = loop->stmt();
    auto scope = stmt->scope();
    if (scope) {
        pushstacklist();
        debug_scope.init(this, &local_syms_);
    }

    if (auto init = loop->init())
        EmitBlock(init);

    LoopContext loop_cx;
    loop_cx.stack_scope_id = stack_scope_id();
    loop_cx.heap_scope_id = heap_scope_id();
    ke::SaveAndSet<LoopContext*> push_context(&loop_, &loop_cx);

    auto advance = loop->advance();
    if (advance) {
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

        if (loop->cond())
            EmitTest(loop->cond(), false, &loop_cx.break_to);

        EmitBlock(loop->body());

        if (stmt->has_continue()) {
            __ bind(&loop_cx.continue_to);

            // It's a bit tricky to merge this into the same heap scope as
            // the statement, so we create a one-off scope.
#if 0
            if (advance->tree_has_heap_allocs())
                EnterHeapScope(Flow_None);
#endif

            EmitBlock(loop->advance());

#if 0
            if (advance->tree_has_heap_allocs())
                LeaveHeapScope();
#endif
        }
        __ emit(OP_JUMP, &top);
    } else if (!stmt->never_taken()) {
        // continue:
        //   <cond>
        //   jf break
        //   <body>
        //   jmp continue
        // break:
        __ bind(&loop_cx.continue_to);

        if (loop->cond())
            EmitTest(loop->cond(), false, &loop_cx.break_to);

        EmitBlock(loop->body());

        __ emit(OP_JUMP, &loop_cx.continue_to);
    }
    __ bind(&loop_cx.break_to);

    if (scope) {
        debug_scope = {};
        popstacklist(true);
    }
}

void CodeGenerator::EmitSwitch(ir::Switch* insn) {
    EmitValue(insn->cond());

    Label exit_label;
    Label table_label;
    __ emit(OP_SWITCH, &table_label);

    // Note: we use map for ordering so the case table is sorted.
    std::map<cell, Label> case_labels;

    for (const auto& [labels, block] : insn->cases()) {
        Label label;
        __ bind(&label);
        for (const auto& label_value : labels)
            case_labels.emplace(label_value, label);

        EmitBlock(block);
        __ emit(OP_JUMP, &exit_label);
    }

    Label default_label;
    Label* defcase = &exit_label;
    if (insn->default_case()) {
        __ bind(&default_label);

        EmitBlock(insn->default_case());
        __ emit(OP_JUMP, &exit_label);

        defcase = &default_label;
    }

    __ bind(&table_label);
    __ casetbl((int)case_labels.size(), defcase);

    for (auto& pair : case_labels)
        __ casetbl_entry(pair.first, &pair.second);

    __ bind(&exit_label);
}

void CodeGenerator::EmitFunction(ir::Function* fun) {
    assert(fun->is_live());

    FunctionDecl* decl = fun->decl();
    if (decl->is_native())
        return;

    assert(fun->body());

    ke::SaveAndSet<ir::Function*> set_fun(&fun_, fun);

    // Minimum 16 cells for general slack.
    current_memory_ = 16;
    max_func_memory_ = current_memory_;

    __ bind(&fun->label());
    __ emit(OP_PROC);
    AddDebugLine(fun->decl()->pos().line);
    EmitBreak();
    current_stack_ = 0;

    {
        AutoEnterScope arg_scope(this, &local_syms_);

        /* Stack layout:
         *   base + 0*sizeof(cell)  == previous "base"
         *   base + 1*sizeof(cell)  == function return address
         *   base + 2*sizeof(cell)  == number of arguments
         *   base + 3*sizeof(cell)  == first argument of the function
         * So the offset of each argument is "(argcnt+3) * sizeof(cell)".
         *
         * Since arglist has an empty terminator at the end, we actually add 2.
         */
        cell_t offset = 0;
        for (const auto& arg : fun->argv()) {
            arg->set_addr((offset + 3) * sizeof(cell_t));
            offset++;
        }

#if 0
        for (const auto& fun_arg : info->args())
            EnqueueDebugSymbol(fun_arg, asm_.position());
#endif

        pushstacklist();
        if (fun->num_slots()) {
            temp_slots_base_ = markstack(decl, MEMUSE_STATIC, fun->num_slots());
            __ emit(OP_STACK, -4 * cell_t(fun->num_slots()));
        } else {
            temp_slots_base_ = 0;
        }

        EmitBlock(fun->body());

        popstacklist(false);
    }

    assert(!has_stack_or_heap_scopes());

    // :TODO: remove this!
    __ emit(OP_CONST_PRI, 0);
    __ emit(OP_RETN);

    // If return keyword is missing, we added it in the semantic pass.
    __ emit(OP_ENDPROC);

    stack_scopes_.clear();
    heap_scopes_.clear();

    fun->set_pcode_end(asm_.pc());
    fun->set_max_local_stack(max_func_memory_);

    // In case there is no callgraph, we still need to track which function has
    // the biggest stack.
    max_script_memory_ = std::max(max_script_memory_, max_func_memory_);
}

void CodeGenerator::EmitBreak() {
    if (last_break_op_ && *last_break_op_ == asm_.position())
        return;
    __ emit(OP_BREAK);
    last_break_op_.init(asm_.position());
}

void CodeGenerator::EmitCall(ir::Function* fun, cell nargs) {
    assert(fun->is_live());

    if (fun->decl()->is_native()) {
        if (!fun->label().bound()) {
            __ bind_to(&fun->label(), native_list_.size());
            native_list_.emplace_back(fun);
        }
        __ sysreq_n(&fun->label(), nargs);
    } else {
        __ emit(OP_PUSH_C, nargs);
        __ emit(OP_CALL, &fun->label());

        auto node = callgraph_.find(fun_);
        if (node == callgraph_.end())
            callgraph_.emplace(fun_, tr::vector<ir::Function*>{fun});
        else
            node->second.emplace_back(fun);
    }

    max_func_memory_ = std::max(max_func_memory_, current_memory_ + nargs);
}

#if 0
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
    assert(user_op.sym->ident() == iFUNCTN);
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
#endif

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
    if (flow_type == Flow_None || flow_type == Flow_Mixed) {
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

void
CodeGenerator::pushstacklist()
{
    EnterMemoryScope(stack_scopes_);
}

cell_t CodeGenerator::markstack(ParseNode* node, MemuseType type, int size) {
    current_stack_ += size;
    AllocInScope(node, stack_scopes_.back(), type, size);
    return -current_stack_ * sizeof(cell);
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
CodeGenerator::popstacklist(bool codegen)
{
    if (codegen)
        modstk_for_scope(stack_scopes_.back());
    current_stack_ -= PopScope(stack_scopes_);
}

void CodeGenerator::LinkPublicFunction(ir::Function* fun, uint32_t id) {
    __ bind_to(&fun->public_id(), id);
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
    ir::Function* caller = caller_iter->first;
    tr::vector<ir::Function*> targets = std::move(caller_iter->second);
    caller_iter = callgraph_.erase(caller_iter);

    int max_child_stack = 0;
    while (!targets.empty()) {
        auto target = ke::PopBack(&targets);
        if (!target->max_callee_stack()) {
            auto iter = callgraph_.find(target);
            if (iter != callgraph_.end()) {
                if (!ComputeStackUsage(iter))
                    return false;
            }
        }

        auto local_stack = target->max_local_stack();
        auto callee_stack = target->max_callee_stack();
        if (!ke::IsUint32AddSafe(local_stack, callee_stack) ||
            local_stack + callee_stack >= kMaxCells)
        {
            report(token_pos_t{}, 431);
            return false;
        }

        max_child_stack = std::max(max_child_stack, local_stack + callee_stack);

        // Assign this each iteration so we at least have something useful if
        // we hit a recursive case.
        caller->set_max_callee_stack(max_child_stack);
    }

    auto local_stack = caller->max_local_stack();
    auto callee_stack = caller->max_callee_stack();
    if (!ke::IsUint32AddSafe(local_stack, callee_stack) ||
        local_stack + callee_stack >= kMaxCells)
    {
        report(token_pos_t{}, 431);
        return false;
    }

    max_script_memory_ = std::max(caller->max_local_stack() + caller->max_callee_stack(),
                                  max_script_memory_);
    return true;
}

bool CodeGenerator::ComputeStackUsage() {
    if (callgraph_.empty())
        return true;

    return ComputeStackUsage(callgraph_.begin());
}

cell_t CodeGenerator::GetSlotAddr(uint32_t slot) {
    assert(slot < fun_->num_slots());
    assert(temp_slots_base_ != 0);
    return temp_slots_base_ + slot * sizeof(cell_t);
}

} // namespace cc
} // namespace sp
