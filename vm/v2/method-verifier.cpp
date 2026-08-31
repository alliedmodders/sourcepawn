// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// Copyright (C) 2006-2015 AlliedModders LLC
//
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#include "v2/method-verifier.h"

#include <assert.h>
#include <limits.h>

#include <amtl/am-vector.h>
#include "binary-reader.h"
#include "environment.h"
#include "graph-builder.h"
#include "v2/opcodes.h"
#include "v2/runtime.h"

namespace sp::v2 {

using namespace ke;

MethodVerifier::MethodVerifier(Runtime* rt, uint32_t method_index)
 : rt_(rt),
   smx_(rt->image()),
   block_(nullptr),
   method_index_(method_index),
   datSize_(rt_->image()->DescribeData().length),
   max_stack_(0),
   code_(nullptr),
   cip_(nullptr),
   prev_cip_(nullptr),
   stop_at_(nullptr)
{
    code_version_ = rt_->image()->DescribeCode().version;
    code_features_ = rt_->image()->DescribeCode().features;
}

const TypeDesc* MethodVerifier::cell_type() const {
    return rt_->GetPrimitiveType(TypeKind::Int32);
}

const TypeDesc* MethodVerifier::any_type() const {
    return rt_->GetPrimitiveType(TypeKind::Any);
}

const TypeDesc* MethodVerifier::int64_type() const {
    return rt_->GetPrimitiveType(TypeKind::Int64);
}

const TypeDesc* MethodVerifier::float32_type() const {
    return rt_->GetPrimitiveType(TypeKind::Float32);
}

ke::RefPtr<ControlFlowGraph>
MethodVerifier::verify() {
    method_ = smx_->GetMethod(method_index_);
    assert(method_);
    assert(!(method_->flags & kRttiMethod_Native));

    auto& code = rt_->code();
    code_ = code.bytes + method_->pcode_start;
    stop_at_ = code.bytes + method_->pcode_end;

    if (!verifyLocalSlots())
        return nullptr;

    GraphBuilder gb(rt_, method_);
    graph_ = gb.build();
    if (!graph_)
        return nullptr;

    AutoClearBlockData<VerifyData> acbd(graph_);

    for (auto iter = graph_->rpoBegin(); iter != graph_->rpoEnd(); iter++) {
        block_ = *iter;
        if (!handleJoins())
            return nullptr;

        prev_cip_ = nullptr;

        cip_ = block_->start();
        while (cip_ < block_->end()) {
            insn_ = cip_;
            OPCODE op = (OPCODE)*cip_++;
            if (!verifyOp(op))
                return nullptr;
            prev_cip_ = insn_;
        }
    }

    // Verify loop headers.
    for (const auto& block : verify_joins_) {
        if (!verifyJoins(block))
            return nullptr;
    }

    if (max_stack_ > INT_MAX / 4) {
        reportError(SP_ERROR_STACKLOW);
        return nullptr;
    }

    return graph_;
}

bool
MethodVerifier::verifyOp(OPCODE op) {
    VerifyData* v = block_->data<VerifyData>();
    switch (op) {
        case OP_NOP:
            return true;

        case OP_LOAD_I_I32:
        case OP_LOAD_I_F32: {
            const TypeDesc* addr;
            if (!popStack(&addr))
                return false;
            if (!addr->IsReference())
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            return pushStack(addr->ref_type());
        }

        case OP_LOAD_I_I64: {
            const TypeDesc* addr;
            if (!popStack(&addr))
                return false;
            if (!addr->IsReference() || !addr->ref_type()->IsInt64())
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            return pushStack(int64_type());
        }

        case OP_LOAD_ELEM_I32:
        case OP_LOAD_ELEM_F32:
        case OP_LOAD_ELEM_I64:
        case OP_LOAD_ELEM_U8:
        case OP_LOAD_ELEM_A: {
            if (!popInt32())
                return false;
            const TypeDesc* base;
            if (!popStack(&base))
                return false;
            if (!base->IsArrayish())
                return reportError(SP_ERROR_INSTRUCTION_PARAM);

            const TypeDesc* elt = base->array_elt();
            if (op == OP_LOAD_ELEM_I64) {
                if (!elt->IsInt64())
                    return reportError(SP_ERROR_INSTRUCTION_PARAM);
                return pushStack(int64_type());
            }
            if (op == OP_LOAD_ELEM_U8)
                return pushStack(cell_type());
            return pushStack(elt);
        }

        case OP_STOR_ELEM_I32:
        case OP_STOR_ELEM_F32:
        case OP_STOR_ELEM_I64:
        case OP_STOR_ELEM_U8: {
            const TypeDesc* val;
            if (!popStack(&val))
                return false;
            if (!popInt32())
                return false;
            const TypeDesc* base;
            if (!popStack(&base))
                return false;
            if (!base->IsArrayish())
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            return ValidateStore(base->array_elt(), val);
        }

        case OP_STOR_I_I32:
        case OP_STOR_I_F32: {
            const TypeDesc* val;
            if (!popStack(&val))
                return false;
            const TypeDesc* addr;
            if (!popStack(&addr))
                return false;
            if (!addr->IsReference())
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            return ValidateStore(addr->ref_type(), val);
        }

        case OP_STOR_I_I64: {
            const TypeDesc* val;
            if (!popStack(&val))
                return false;
            const TypeDesc* addr;
            if (!popStack(&addr))
                return false;
            if (!addr->IsReference() || !addr->ref_type()->IsInt64())
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            return ValidateStore(addr->ref_type(), val);
        }

        case OP_IDXADDR:
        case OP_SLICE: {
            // Pops index, pops base address, pushes result.
            if (!popInt32())
                return false;

            const TypeDesc* base;
            if (!popStack(&base))
                return false;

            if (!base->IsArrayish())
                return reportError(SP_ERROR_INSTRUCTION_PARAM);

            if (op == OP_SLICE)
                return pushStack(base);

            return pushStack(rt_->GetReferenceType(base->array_elt()));
        }

        case OP_SHL:
        case OP_SHR:
        case OP_SSHR:
        case OP_AND:
        case OP_OR:
        case OP_XOR: {
            const TypeDesc *b, *a;
            if (!popStack(&b) || !popStack(&a))
                return false;
            if (a->kind() == TypeKind::Int64 && b->kind() == TypeKind::Int64) {
                return pushStack(int64_type());
            } else if (checkIntOrFloat(a) && checkIntOrFloat(b)) {
                return pushStack(cell_type());
            } else {
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            }
        }

        case OP_SMUL:
        case OP_ADD:
        case OP_SUB:
        case OP_SDIV:
        case OP_SMOD: {
            const TypeDesc *b, *a;
            if (!popStack(&b) || !popStack(&a))
                return false;
            if (a->kind() == TypeKind::Float32 && b->kind() == TypeKind::Float32) {
                return pushStack(float32_type());
            } else if (a->kind() == TypeKind::Int64 && b->kind() == TypeKind::Int64) {
                return pushStack(int64_type());
            } else if (checkIntOrFloat(a) && checkIntOrFloat(b)) {
                return pushStack(cell_type());
            } else {
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            }
        }

        case OP_EQ:
        case OP_NEQ:
        case OP_SLESS:
        case OP_SLEQ:
        case OP_SGRTR:
        case OP_SGEQ: {
            const TypeDesc *b, *a;
            if (!popStack(&b) || !popStack(&a))
                return false;
            if (a->kind() == TypeKind::Float32 && b->kind() == TypeKind::Float32) {
                return pushStack(cell_type());
            } else if (a->kind() == TypeKind::Int64 && b->kind() == TypeKind::Int64) {
                return pushStack(cell_type());
            } else if (checkCell(a) && checkCell(b)) {
                return pushStack(cell_type());
            } else {
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            }
        }



        case OP_NOT:
            return popIntOrFloat() && pushStack(cell_type());

        case OP_INC:
        case OP_DEC: {
            const TypeDesc* a;
            if (!popStack(&a))
                return false;
            if (a->kind() == TypeKind::Int64)
                return pushStack(int64_type());
            else if (checkIntOrFloat(a))
                return pushStack(a);
            else
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
        }

        case OP_NEG:
        case OP_INVERT: {
            const TypeDesc* a;
            if (!popStack(&a))
                return false;
            if (a->kind() == TypeKind::Int64)
                return pushStack(int64_type());
            else if (checkIntOrFloat(a))
                return pushStack(a);
            else
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
        }

        case OP_CVT_F32:
            return popIntOrFloat() && pushStack(float32_type());

        case OP_TEST: {
            const TypeDesc* a;
            if (!popStack(&a))
                return false;
            if (a->kind() == TypeKind::Int64)
                return pushStack(cell_type());
            else if (checkIntOrFloat(a))
                return pushStack(cell_type());
            else
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
        }

        case OP_TRUNCATE_I64:
            return popStack(TypeKind::Int64) && pushStack(cell_type());



        case OP_DUP:
            if (v->stack.empty())
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            return pushStack(v->stack.back());

        case OP_SWAP: {
            const TypeDesc *t1, *t2;
            if (!popStack(&t1) || !popStack(&t2))
                return false;
            return pushStack(t1) && pushStack(t2);
        }

        case OP_POP:
            return popStack(1);

        case OP_ADDR_S:
        {
            cell_t offset = readInt16();
            const TypeDesc* td = verifyStackOffset(offset);
            if (!td)
                return false;
            return pushStack(rt_->GetReferenceType(td));
        }

        case OP_LOAD_S:
        {
            cell_t offset = readInt16();
            auto td = verifyStackOffset(offset);
            if (!td)
                return false;
            return pushStack(td);
        }

        case OP_STOR_S:
        {
            cell_t offset = readInt16();
            auto local = verifyStackOffset(offset);
            if (!local)
                return false;
            const TypeDesc* td;
            if (!popStack(&td))
                return false;
            return ValidateStore(local, td);
        }

        case OP_STOR_S_C: {
            cell_t offset = readInt16();
            readCell();
            auto local = verifyStackOffset(offset);
            if (!local)
                return false;
            return ValidateStore(local, cell_type());
        }

        case OP_CVT_I64:
            return popInt32() && pushStack(int64_type());




        case OP_LOAD_GLB:
        {
            uint16_t index = read<uint16_t>();
            auto td = verifyGlobalIndex(index);
            if (!td)
                return false;
            return pushStack(td);
        }

        case OP_STOR_GLB:
        {
            uint16_t index = read<uint16_t>();
            auto global = verifyGlobalIndex(index);
            if (!global)
                return false;
            const TypeDesc* val;
            if (!popStack(&val))
                return false;
            return ValidateStore(global, val);
        }

        case OP_ADDR_GLB:
        {
            uint16_t index = read<uint16_t>();
            auto td = verifyGlobalIndex(index);
            if (!td)
                return false;
            return pushStack(rt_->GetReferenceType(td));
        }

        case OP_LOAD_I_U8: {
            const TypeDesc* addr;
            if (!popStack(&addr))
                return false;
            if (!addr->IsReference())
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            return pushStack(cell_type());
        }

        case OP_STOR_I_U8: {
            const TypeDesc* val;
            if (!popStack(&val))
                return false;
            const TypeDesc* addr;
            if (!popStack(&addr))
                return false;
            if (!addr->IsReference())
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            return ValidateStore(addr->ref_type(), val);
        }

        case OP_PUSH_C: {
            readCell();
            return pushStack(cell_type());
        }

        case OP_PUSH_C_I8: {
            read<int8_t>();
            return pushStack(cell_type());
        }

        case OP_PUSH_C_I64: {
            read<int64_t>();
            return pushStack(int64_type());
        }

        case OP_PUSH_C_F32: {
            read<float>();
            return pushStack(float32_type());
        }

        case OP_CALL:
        case OP_CALLN: {
            uint32_t method_index = (uint32_t)readCell();
            uint32_t arg_count;
            const smx_rtti_method* method = smx_->GetMethod(method_index);

            if (op == OP_CALLN) {
                arg_count = (uint8_t)read<uint8_t>();
            } else {
                auto parser = smx_->GetTypeParser(method->signature);
                if (!parser.ReadFunctionSignatureArgCount(&arg_count))
                    return reportError(SP_ERROR_INVALID_INSTRUCTION);
            }

            if (!verifyCallIndex(method_index))
                return false;

            // The interpreter pushes the argument count onto the stack before
            // resolving the call.
            if (!pushStack(cell_type()))
                return false;
            if (!popStack(arg_count + 1))
                return false;

            if (!smx_->IsVoidMethod(method)) {
                auto parser = smx_->GetTypeParser(method->signature);
                uint32_t unused_argc;
                parser.ReadFunctionSignatureArgCount(&unused_argc);
                uint8_t variadic;
                parser.GetByte(&variadic);
                if (variadic == cb::kLegacyVariadic)
                    parser.NextByte();
                auto td = rt_->LoadType(parser);
                if (!td || !pushStack(td))
                    return false;
            }
            if (collect_func_refs_)
                collect_func_refs_(method_index);
            return true;
        }

        case OP_JUMP:
            readCell();
            return true;

        case OP_JZER:
        case OP_JNZ:
            readCell();
            return popCell();

        case OP_JEQ:
        case OP_JNEQ:
        case OP_JSLESS:
        case OP_JSLEQ:
        case OP_JSGRTR:
        case OP_JSGEQ:
            readCell();
            return popCell() && popCell();


        case OP_SWITCH:
            cip_ = insn_ + GetSwitchOpcodeSize(insn_);
            return popCell();

        case OP_COPYARRAY: {
            const TypeDesc *src, *dest;
            if (!popStack(&src) || !popStack(&dest))
                return false;
            if (!src->IsArrayish() || dest->kind() != TypeKind::FixedArray)
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            if (src->kind() == TypeKind::FixedArray && src->array_size() > dest->array_size())
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            if (src->array_elt()->element_size() != dest->array_elt()->element_size())
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            return true;
        }

        case OP_NEWARRAY: {
            uint32_t type_id = read<uint32_t>();
            auto td = rt_->LoadTypeFromId(type_id);
            if (!td)
                return false;
            if (td->kind() == TypeKind::Array) {
                if (!popInt32())
                    return false;
            } else if (td->kind() != TypeKind::FixedArray) {
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            }
            return pushStack(td);
        }

        case OP_NEWBULKARRAY: {
            uint8_t count = read<uint8_t>();
            if (count < 1)
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            uint32_t type_id = read<uint32_t>();
            auto td = rt_->LoadTypeFromId(type_id);
            if (!td)
                return false;

            uint32_t dynamic_rank = 0;
            for (auto iter = td; iter->kind() == TypeKind::Array; iter = iter->array_elt())
                dynamic_rank++;

            if (count > dynamic_rank)
                return reportError(SP_ERROR_INSTRUCTION_PARAM);

            for (uint32_t i = 0; i < count; i++) {
                if (!popInt32())
                    return false;
            }
            return pushStack(td);
        }

        case OP_FILLARRAY: {
            const TypeDesc* td;
            if (!popStack(&td))
                return false;
            if (!td->IsArrayish())
                return reportError(SP_ERROR_INSTRUCTION_PARAM);

            uint32_t data_offs = read<uint32_t>();
            if (!smx_->IsValidDataOffset(data_offs))
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            BinaryReader reader = smx_->GetDataReader(data_offs);
            auto bytes = reader.readCompactUint32();
            if (!bytes || !reader.canRead(*bytes))
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            return true;
        }

        case OP_HEAP_SAVE:
            v->heap_scope_depth++;
            return true;

        case OP_HEAP_RESTORE:
            if (!v->heap_scope_depth)
                return reportError(SP_ERROR_INVALID_INSTRUCTION);
            v->heap_scope_depth--;
            return true;

        case OP_RETN: {
            if (return_type_->kind() == TypeKind::Void)
                return reportError(SP_ERROR_INVALID_INSTRUCTION);
            const TypeDesc* td;
            if (!popStack(&td))
                return false;
            if (!ValidateStore(return_type_, td))
                return false;
            block_->heap_scope_depth() = v->heap_scope_depth;
            return true;
        }

        case OP_RETV:
            if (return_type_->kind() != TypeKind::Void)
                return reportError(SP_ERROR_INVALID_INSTRUCTION);
            block_->heap_scope_depth() = v->heap_scope_depth;
            return true;

        case OP_LOAD_FN: {
            uint32_t method_index = readCell();
            if (!smx_->GetMethod(method_index))
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            return pushStack(cell_type());
        }

        case OP_LOAD_STR: {
            uint32_t offset = read<uint16_t>();
            if (!verifyDatString(offset))
                return false;
            return pushStack(rt_->GetStringLitType(offset));
        }

        case OP_ARRAY_TO_NATIVE: {
            const TypeDesc* td;
            if (!popStack(&td))
                return false;
            if (!td->IsArrayish())
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            return pushStack(td);
        }

        default:
            // Should have been caught earlier.
            return reportError(SP_ERROR_INVALID_INSTRUCTION);
    }
}

static inline bool IsPodType(const TypeDesc* type) {
    switch (type->kind()) {
        case TypeKind::Bool:
        case TypeKind::Int32:
        case TypeKind::Int64:
        case TypeKind::Float32:
        case TypeKind::Char8:
        case TypeKind::Any:
        case TypeKind::TopFunction:
            return true;
        default:
            return false;
    }
}

bool MethodVerifier::verifyJoin(VerifyData* first, VerifyData* other) {
    if (first->stack.size() != other->stack.size()) {
        return reportError(SP_ERROR_INSTRUCTION_PARAM);
    }

    for (size_t i = 0; i < first->stack.size(); i++) {
        const TypeDesc* t1 = first->stack[i];
        const TypeDesc* t2 = other->stack[i];

        if (t1 == t2)
            continue;

        if (IsPodType(t1) && IsPodType(t2)) {
            if (t1->slot_size() != t2->slot_size())
                return reportError(SP_ERROR_INSTRUCTION_PARAM);

            first->stack[i] = any_type();
        } else if (t1->IsArrayish() && t2->IsArrayish() && t1->array_elt() == t2->array_elt()) {
            first->stack[i] = rt_->GetArrayType(t1->array_elt());
        } else {
            return reportError(SP_ERROR_INSTRUCTION_PARAM);
        }
    }

    if (first->heap_scope_depth != other->heap_scope_depth) {
        return reportError(SP_ERROR_INSTRUCTION_PARAM);
    }
    return true;
}

bool
MethodVerifier::mergeTracker(Block* block, VerifyData* other) {
    VerifyData* join = block->data<VerifyData>();
    return verifyJoin(join, other);
}

bool
MethodVerifier::handleJoins() {
    if (block_->predecessors().empty())
        return true;

    bool verify_later = false;

    bool found_pred = false;
    for (size_t i = 0; i < block_->predecessors().size(); i++) {
        Block* pred = block_->predecessors()[i];

        // Backedges won't have been visited yet, so we'll have to verify this
        // block again later. However, we keep going to get at least one
        // predecessor to use for our initial stack balance.
        if (pred->id() >= block_->id()) {
            verify_later = true;
            continue;
        }

        VerifyData* pred_data = pred->data<VerifyData>();
        if (!found_pred) {
            // Inherit everything from the first already-visited preceding block.
            VerifyData* data = block_->data<VerifyData>();
            *data = *pred_data;

            // Save the entry state.
            data->entry = std::make_unique<VerifyData>(*pred_data);

            found_pred = true;
            continue;
        }

        if (!mergeTracker(block_, pred_data))
            return false;
    }

    if (verify_later)
        verify_joins_.push_back(block_);

    // If the block had no incoming edges other than backedges, then this would
    // be a backedge to the entry block, which we allow in V2 as we don't have
    // entry opcodes anymore (PROC and BREAK are gone).  In this case, we have
    // to ensure that initial verify data is present.
    if (!found_pred) {
        assert(verify_later);

        VerifyData* data = block_->data<VerifyData>();
        if (!data->entry) {
            assert(block_ == graph_->entry());
            data->entry = std::make_unique<VerifyData>(*data);
        }
        return true;
    }
    return true;
}

bool
MethodVerifier::verifyJoins(Block* block) {
    VerifyData* join_data = block->data<VerifyData>();
    VerifyData* entry = join_data->entry.get();

    for (size_t i = 0; i < block->predecessors().size(); i++) {
        Block* other_pred = block->predecessors()[i];
        VerifyData* other_edge = other_pred->data<VerifyData>();

        if (!verifyJoin(entry, other_edge))
            return false;
    }
    return true;
}

bool
MethodVerifier::pushStack(const TypeDesc* type) {
    VerifyData* v = block_->data<VerifyData>();
    v->stack.push_back(type);

    if (v->stack.size() > max_eval_stack_depth_)
        max_eval_stack_depth_ = (uint32_t)v->stack.size();

    v->stack_bytes += type->slot_size();
    if (v->stack_bytes > max_eval_stack_bytes_)
        max_eval_stack_bytes_ = v->stack_bytes;

    if (v->stack.size() > max_stack_)
        max_stack_ = (uint32_t)v->stack.size();
    return true;
}

bool
MethodVerifier::popStack(TypeKind kind) {
    const TypeDesc* other;
    if (!popStack(&other))
        return false;
    if (other->kind() != kind) {
        return reportError(SP_ERROR_INSTRUCTION_PARAM);
    }
    return true;
}

bool
MethodVerifier::popStack(const TypeDesc** type) {
    VerifyData* v = block_->data<VerifyData>();
    if (v->stack.empty()) {
        return reportError(SP_ERROR_INSTRUCTION_PARAM);
    }
    *type = v->stack.back();
    v->stack_bytes -= (*type)->slot_size();
    v->stack.pop_back();
    return true;
}

bool MethodVerifier::checkCell(const TypeDesc* td) {
    switch (td->kind()) {
        case TypeKind::Void:
        case TypeKind::Int64:
            return false;
        default:
            return true;
    }
}

bool MethodVerifier::checkIntOrFloat(const TypeDesc* td) {
    switch (td->kind()) {
        case TypeKind::Bool:
        case TypeKind::Int32:
        case TypeKind::Float32:
        case TypeKind::Char8:
        case TypeKind::Any:
            return true;
        default:
            return false;
    }
}

bool MethodVerifier::popCell() {
    const TypeDesc* td;
    if (!popStack(&td))
        return false;
    if (!checkCell(td))
        return reportError(SP_ERROR_INSTRUCTION_PARAM);
    return true;
}

bool MethodVerifier::popIntOrFloat() {
    const TypeDesc* td;
    if (!popStack(&td))
        return false;
    if (!checkIntOrFloat(td))
        return reportError(SP_ERROR_INSTRUCTION_PARAM);
    return true;
}

bool MethodVerifier::popInt32() {
    const TypeDesc* td;
    if (!popStack(&td))
        return false;
    switch (td->kind()) {
        case TypeKind::Bool:
        case TypeKind::Int32:
        case TypeKind::Char8:
        case TypeKind::Any:
            return true;
        default:
            return reportError(SP_ERROR_INSTRUCTION_PARAM);
    }
}

bool
MethodVerifier::popStack(uint32_t num_operands) {
    VerifyData* v = block_->data<VerifyData>();
    if (v->stack.size() < num_operands) {
        return reportError(SP_ERROR_INSTRUCTION_PARAM);
    }
    for (uint32_t i = 0; i < num_operands; i++) {
        const TypeDesc* type = v->stack.back();
        v->stack_bytes -= type->slot_size();
        v->stack.pop_back();
    }
    return true;
}

const TypeDesc* MethodVerifier::verifyStackOffset(cell_t offset) {
    // Modern stack is typed.
    if (offset < 0) {
        uint32_t arg_slot = -offset - 1;
        if (arg_slot >= arg_count_) {
            reportError(SP_ERROR_INSTRUCTION_PARAM);
            return nullptr;
        }
        return arg_types_[arg_slot];
    } else {
        if (offset >= (cell_t)local_types_.size()) {
            reportError(SP_ERROR_INSTRUCTION_PARAM);
            return nullptr;
        }
        return local_types_[offset];
    }
}

bool MethodVerifier::verifyDatAddress(cell_t offset) {
    if (offset < 0 || size_t(offset) >= datSize_) {
        return reportError(SP_ERROR_INSTRUCTION_PARAM);
    }
    return true;
}

bool MethodVerifier::verifyDatString(uint16_t index) {
    const smx_rtti_table_header* table = smx_->rtti_stringpool();
    if (!table || index >= table->row_count)
        return reportError(SP_ERROR_INSTRUCTION_PARAM);

    // If loaded in data only mode, we skipped verifying the string table.
    if (rt_->data_only()) {
        const smx_rtti_string* row = smx_->getRttiRow<smx_rtti_string>(table, index);
        if (!smx_->ReadDataBlob(row->offset))
            return false;
    }
    return true;
}

const TypeDesc* MethodVerifier::verifyGlobalIndex(uint16_t index) {
    const smx_rtti_table_header* globals = smx_->rtti_globals();
    if (!globals || index >= globals->row_count) {
        reportError(SP_ERROR_INSTRUCTION_PARAM);
        return nullptr;
    }

    const smx_rtti_global* global = smx_->getRttiRow<smx_rtti_global>(globals, index);
    return rt_->LoadTypeFromId(global->type_id);
}

bool
MethodVerifier::verifyDimensionCount(cell_t ndims) {
    if (ndims <= 0) {
        return reportError(SP_ERROR_INSTRUCTION_PARAM);
    }
    return true;
}

bool
MethodVerifier::verifyParamCount(cell_t nparams) {
    if (nparams < 0 || nparams > SP_MAX_CALL_ARGUMENTS) {
        return reportError(SP_ERROR_INSTRUCTION_PARAM);
    }
    return true;
}

bool MethodVerifier::verifyCallIndex(uint32_t method_index) {
    if (!smx_->GetMethod(method_index)) {
        return reportError(SP_ERROR_INSTRUCTION_PARAM);
    }
    return true;
}

void
MethodVerifier::collectExternalFuncRefs(const ExternalFuncRefCallback& callback) {
    collect_func_refs_ = callback;
}

bool
MethodVerifier::reportError(int err) {
    // Break here to find why verification failed.
    rt_->ReportErrorNumber(err);
    return false;
}

bool MethodVerifier::verifyLocalSlots() {
    if (!method_)
        return reportError(SP_ERROR_FILE_FORMAT);

    auto parser = rt_->image()->GetTypeParser(method_->signature);
    if (!parser.ReadFunctionSignatureArgCount(&arg_count_))
        return reportError(SP_ERROR_FILE_FORMAT);

    uint8_t variadic;
    if (!parser.GetByte(&variadic))
        return reportError(SP_ERROR_FILE_FORMAT);
    if (variadic == cb::kLegacyVariadic)
        parser.NextByte();

    // Read return type
    uint8_t type_byte;
    if (!parser.GetByte(&type_byte))
        return reportError(SP_ERROR_FILE_FORMAT);
    if (type_byte == cb::kVoid) {
        parser.NextByte();
        return_type_ = rt_->GetPrimitiveType(TypeKind::Void);
    } else {
        return_type_ = rt_->LoadType(parser);
        if (!return_type_)
            return reportError(SP_ERROR_FILE_FORMAT);
    }

    arg_types_ = ke::FixedArray<const TypeDesc*>(arg_count_);
    for (uint32_t i = 0; i < arg_count_; i++) {
        auto td = rt_->LoadArgType(parser);
        if (!td)
            return reportError(SP_ERROR_FILE_FORMAT);
        arg_types_[i] = td;
    }

    if (!method_->locals)
        return true;

    parser = rt_->image()->GetTypeParser(method_->locals);

    uint16_t count;
    if (!parser.ReadLocalSlotCount(&count))
        return reportError(SP_ERROR_FILE_FORMAT);

    local_types_ = ke::FixedArray<const TypeDesc*>(count);
    for (uint16_t i = 0; i < count; i++) {
        auto td = rt_->LoadType(parser);
        if (!td)
            return false;
        // :TODO: forbid references
        local_types_[i] = td;
   }
   return true;
}

bool MethodVerifier::ValidateStore(const TypeDesc* dest, const TypeDesc* src) {
    if (IsPodType(dest)) {
        if (!IsPodType(src) || dest->slot_size() != src->slot_size())
            return reportError(SP_ERROR_INVALID_INSTRUCTION);
        return true;
    }

    switch (dest->kind()) {
        case TypeKind::Array:
        case TypeKind::FixedArray:
        case TypeKind::ArraySlice:
        case TypeKind::Reference:
            if (dest != src)
                return reportError(SP_ERROR_INVALID_INSTRUCTION);
            return true;
        default:
            assert(false);
            return false;
    }
}

} // namespace sp::v2
