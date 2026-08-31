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
#include <assert.h>
#include <limits.h>

#include <amtl/am-vector.h>
#include "binary-reader.h"
#include "environment.h"
#include "graph-builder.h"
#include "v2/method-verifier.h"
#include "v2/opcodes.h"
#include "v2/runtime.h"

namespace sp::v2 {

using namespace ke;

MethodVerifier::MethodVerifier(Runtime* rt, uint32_t method_index)
 : rt_(rt),
   smx_(rt->image()),
   block_(nullptr),
   method_index_(method_index),
   memSize_(rt_->HeapSize()),
   datSize_(rt_->image()->DescribeData().length),
   heapSize_(memSize_ - datSize_),
   max_stack_(0),
   code_(nullptr),
   cip_(nullptr),
   prev_cip_(nullptr),
   stop_at_(nullptr)
{
    assert(datSize_ < memSize_);
    assert(heapSize_ <= memSize_ - datSize_);

    code_version_ = rt_->image()->DescribeCode().version;
    code_features_ = rt_->image()->DescribeCode().features;
}

ke::RefPtr<ControlFlowGraph>
MethodVerifier::verify() {
    method_ = smx_->GetMethod(method_index_);
    assert(method_);

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

        case OP_LOAD_I:
            return popStack(OperandType::Cell) && pushStack(OperandType::Cell);

        case OP_LOAD_I_I64:
            return popStack(OperandType::Cell) && pushStack(OperandType::Int64);

        case OP_STOR_I:
            return popStack(OperandType::Cell) && popStack(OperandType::Cell);

        case OP_STOR_I_I64:
            return popStack(OperandType::Int64) && popStack(OperandType::Cell);

        case OP_IDXADDR:
            // rank_size(uint8_t), bounds(uint32_t)
            read<uint8_t>();
            read<int32_t>();
            // Pops index, pops base address, pushes result.
            return popStack(OperandType::Cell) && popStack(OperandType::Cell) && pushStack(OperandType::Cell);

        case OP_SHL:
        case OP_SHR:
        case OP_SSHR:
        case OP_SMUL:
        case OP_ADD:
        case OP_SUB:
        case OP_AND:
        case OP_OR:
        case OP_XOR:
        case OP_EQ:
        case OP_NEQ:
        case OP_SLESS:
        case OP_SLEQ:
        case OP_SGRTR:
        case OP_SGEQ:
        case OP_SDIV_I32:
        case OP_SMOD_I32:
        case OP_MUL_F32:
        case OP_DIV_F32:
        case OP_ADD_F32:
        case OP_SUB_F32:
        case OP_EQ_F32:
        case OP_NEQ_F32:
        case OP_LESS_F32:
        case OP_LEQ_F32:
        case OP_GRTR_F32:
        case OP_GEQ_F32:
        case OP_MOD_F32:
            return popStack(OperandType::Cell) && popStack(OperandType::Cell) && pushStack(OperandType::Cell);

        case OP_NOT:
        case OP_NEG:
        case OP_INVERT:
        case OP_INC:
        case OP_DEC:
        case OP_NEG_F32:
        case OP_CVT_F32:
        case OP_TEST_F32:
            return popStack(OperandType::Cell) && pushStack(OperandType::Cell);

        case OP_STRADJUST:
            return popStack(OperandType::Cell) && pushStack(OperandType::Cell);

        case OP_TRUNCATE_I64:
        case OP_TEST_I64:
            return popStack(OperandType::Int64) && pushStack(OperandType::Cell);

        case OP_SLESS_I64:
        case OP_SLEQ_I64:
        case OP_SGRTR_I64:
        case OP_SGEQ_I64:
        case OP_EQ_I64:
        case OP_NEQ_I64:
            return popStack(OperandType::Int64) && popStack(OperandType::Int64) && pushStack(OperandType::Cell);

        case OP_DUP:
            if (v->stack.empty())
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            return pushStack(v->stack.back());

        case OP_DUP_ROTATE: {
            OperandType t1, t2;
            if (!popStack(&t1) || !popStack(&t2))
                return false;
            return pushStack(t1) && pushStack(t2) && pushStack(t1);
        }

        case OP_SWAP: {
            OperandType t1, t2;
            if (!popStack(&t1) || !popStack(&t2))
                return false;
            return pushStack(t1) && pushStack(t2);
        }

        case OP_POP:
            return popStack(OperandType::Cell);

        case OP_ADDR_S:
        {
            cell_t offset = readInt16();
            if (!verifyStackOffset(offset, 0))
                return false;
            return pushStack(OperandType::Cell);
        }

        case OP_LOAD_S:
        case OP_LREF_S:
        {
            cell_t offset = readInt16();
            if (!verifyStackOffset(offset, sizeof(cell_t)))
                return false;
            return pushStack(OperandType::Cell);
        }

        case OP_LOAD_S_I64:
        {
            cell_t offset = readInt16();
            if (!verifyStackOffset(offset, sizeof(int64_t)))
                return false;
            return pushStack(OperandType::Int64);
        }

        case OP_STOR_S:
        case OP_SREF_S:
        {
            cell_t offset = readInt16();
            if (!verifyStackOffset(offset, sizeof(cell_t)))
                return false;
            return popStack(OperandType::Cell);
        }

        case OP_ZERO_S:
        {
            cell_t offset = readInt16();
            return verifyStackOffset(offset, sizeof(cell_t));
        }

        case OP_ZERO_S_I64:
        {
            cell_t offset = readInt16();
            return verifyStackOffset(offset, sizeof(int64_t));
        }

        case OP_STOR_S_C: {
            cell_t offset = readInt16();
            readCell();
            return verifyStackOffset(offset, sizeof(cell_t));
        }

        case OP_CVT_I64:
            return popStack(OperandType::Cell) && pushStack(OperandType::Int64);

        case OP_INVERT_I64:
        case OP_NEG_I64:
            return popStack(OperandType::Int64) && pushStack(OperandType::Int64);

        case OP_SMUL_I64:
        case OP_ADD_I64:
        case OP_SUB_I64:
        case OP_SHL_I64:
        case OP_SSHR_I64:
        case OP_SHR_I64:
        case OP_OR_I64:
        case OP_AND_I64:
        case OP_XOR_I64:
        case OP_SDIV_I64:
        case OP_SMOD_I64:
            return popStack(OperandType::Int64) && popStack(OperandType::Int64) && pushStack(OperandType::Int64);

        case OP_STOR_S_I64:
        {
            cell_t offset = readInt16();
            if (!verifyStackOffset(offset, sizeof(int64_t)))
                return false;
            return popStack(OperandType::Int64);
        }

        case OP_STOR_S_C_I64: {
            cell_t offset = readInt16();
            readCell();
            readCell();
            return verifyStackOffset(offset, sizeof(int64_t));
        }

        case OP_LOAD_GLB:
        {
            cell_t offset = readCell();
            if (!verifyDatOffset(offset))
                return false;
            return pushStack(OperandType::Cell);
        }

        case OP_LOAD_GLB_I64:
        {
            cell_t offset = readCell();
            if (!verifyDatOffset(offset))
                return false;
            return pushStack(OperandType::Int64);
        }

        case OP_STOR_GLB:
        {
            cell_t offset = readCell();
            if (!verifyDatOffset(offset))
                return false;
            return popStack(OperandType::Cell);
        }

        case OP_STOR_GLB_I64:
        {
            cell_t offset = readCell();
            if (!verifyDatOffset(offset))
                return false;
            return popStack(OperandType::Int64);
        }

        case OP_LODB_I:
        case OP_STRB_I: {
            if (op == OP_LODB_I)
                return popStack(OperandType::Cell) && pushStack(OperandType::Cell);
            return popStack(OperandType::Cell) && popStack(OperandType::Cell);
        }

        case OP_PUSH_C: {
            readCell();
            return pushStack(OperandType::Cell);
        }

        case OP_PUSH_C_I8: {
            read<int8_t>();
            return pushStack(OperandType::Cell);
        }

        case OP_PUSH_C_I64: {
            read<int64_t>();
            return pushStack(OperandType::Int64);
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
            if (!pushStack(OperandType::Cell))
                return false;
            if (!popStack(arg_count + 1))
                return false;

            if (!smx_->IsVoidMethod(method)) {
                if (!pushStack(OperandType::Cell))
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
            return popStack(OperandType::Cell);

        case OP_JEQ:
        case OP_JNEQ:
        case OP_JSLESS:
        case OP_JSLEQ:
        case OP_JSGRTR:
        case OP_JSGEQ:
            readCell();
            return popStack(OperandType::Cell) && popStack(OperandType::Cell);

        case OP_ADD_C:
        case OP_SMUL_C: {
            readCell();
            OperandType type;
            if (!popStack(&type))
                return false;
            return pushStack(type);
        }

        case OP_SWITCH:
            readCell();
            return popStack(OperandType::Cell);

        case OP_CASETBL:
            cip_ = insn_ + GetCaseTableSize(insn_);
            return true;

        case OP_MOVS: {
            cell_t val = readCell();
            if (!verifyMemAmount(val))
                return false;
            return popStack(OperandType::Cell) && popStack(OperandType::Cell);
        }

        case OP_FILL: {
            cell_t val = readCell();
            if (!verifyMemAmount(val))
                return false;
            return popStack(OperandType::Cell) && popStack(OperandType::Cell);
        }

        // Note - STACK and HEAP are verified at runtime.
        case OP_HEAP:
        {
            cell_t value = readCell();
            if (!ke::IsAligned(value, sizeof(cell_t)))
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            if (value < 0)
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            if (value > INT_MAX / 4)
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            return pushStack(OperandType::Cell);
        }

        case OP_GENARRAY:
        case OP_GENARRAY_Z: {
            cell_t ndims = readCell();
            if (!verifyDimensionCount(ndims))
                return false;
            for (cell_t i = 0; i < ndims; i++) {
                if (!popStack(OperandType::Cell))
                    return false;
            }
            return pushStack(OperandType::Cell);
        }

        case OP_INITARRAY: {
            constexpr cell_t kMaxCells = INT_MAX / (2 * (int)sizeof(cell_t));

            if (!popStack(OperandType::Cell))
                return false;

            cell_t addr = readCell();
            cell_t iv_size = readCell();
            cell_t data_copy_size = readCell();
            cell_t data_fill_size = readCell();
            cell_t fill_value = readCell();
            if (iv_size < 0 || data_copy_size < 0 || data_fill_size < 0 || iv_size >= kMaxCells ||
                data_copy_size >= kMaxCells || data_fill_size >= kMaxCells ||
                (!data_fill_size && fill_value) || !ke::IsAligned(addr, sizeof(cell_t))) {
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            }

            cell_t copy_addr = addr + iv_size * sizeof(cell_t);
            cell_t fill_addr = copy_addr + data_copy_size * sizeof(cell_t);
            if (copy_addr < addr || fill_addr < copy_addr ||
                !ke::IsUintAddSafe<uint32_t>(fill_addr, data_fill_size * sizeof(cell_t))) {
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            }

            // If there's nothing to read from DAT, we can early return.
            if (!iv_size && !data_copy_size)
                return true;

            cell_t end_addr = addr + (iv_size + data_copy_size) * sizeof(cell_t);
            if (!verifyDatOffset(addr) || !verifyDatOffset(end_addr - 1))
                return false;
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

        case OP_RETN:
            if (!popStack(OperandType::Cell))
                return false;
            [[fallthrough]];
        case OP_RETV:
            block_->heap_scope_depth() = v->heap_scope_depth;
            return true;

        case OP_LOAD_FN: {
            uint32_t method_index = readCell();
            if (!smx_->GetMethod(method_index))
                return reportError(SP_ERROR_INSTRUCTION_PARAM);
            return pushStack(OperandType::Cell);
        }

        default:
            // Should have been caught earlier.
            return reportError(SP_ERROR_INVALID_INSTRUCTION);
    }
}

bool
MethodVerifier::verifyJoin(VerifyData* first, VerifyData* other) {
    if (first->stack != other->stack) {
        return reportError(SP_ERROR_INSTRUCTION_PARAM);
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
MethodVerifier::pushStack(OperandType type) {
    VerifyData* v = block_->data<VerifyData>();
    v->stack.push_back(type);

    if (v->stack.size() > max_eval_stack_depth_)
        max_eval_stack_depth_ = (uint32_t)v->stack.size();

    v->stack_bytes += (type == OperandType::Int64 ? 8 : 4);
    if (v->stack_bytes > max_eval_stack_bytes_)
        max_eval_stack_bytes_ = v->stack_bytes;

    if (v->stack.size() > max_stack_)
        max_stack_ = (uint32_t)v->stack.size();
    return true;
}

bool
MethodVerifier::popStack(OperandType type) {
    OperandType other;
    if (!popStack(&other))
        return false;
    if (other != type) {
        return reportError(SP_ERROR_INSTRUCTION_PARAM);
    }
    return true;
}

bool
MethodVerifier::popStack(OperandType* type) {
    VerifyData* v = block_->data<VerifyData>();
    if (v->stack.empty()) {
        return reportError(SP_ERROR_INSTRUCTION_PARAM);
    }
    *type = v->stack.back();
    v->stack_bytes -= (*type == OperandType::Int64 ? 8 : 4);
    v->stack.pop_back();
    return true;
}

bool
MethodVerifier::popStack(uint32_t num_operands) {
    VerifyData* v = block_->data<VerifyData>();
    if (v->stack.size() < num_operands) {
        return reportError(SP_ERROR_INSTRUCTION_PARAM);
    }
    for (uint32_t i = 0; i < num_operands; i++) {
        OperandType type = v->stack.back();
        v->stack_bytes -= (type == OperandType::Int64 ? 8 : 4);
        v->stack.pop_back();
    }
    return true;
}

bool MethodVerifier::verifyStackOffset(cell_t offset, uint32_t op_size) {
    // Modern stack is typed.
    if (offset < 0) {
        uint32_t arg_slot = -offset - 1;
        if (arg_slot >= arg_count_)
            return reportError(SP_ERROR_INSTRUCTION_PARAM);
    } else {
        if (offset >= local_sizes_.size())
            return reportError(SP_ERROR_INSTRUCTION_PARAM);
        if (op_size && local_sizes_[offset] != op_size)
            return reportError(SP_ERROR_INSTRUCTION_PARAM);
    }
    return true;
}

bool
MethodVerifier::verifyDatOffset(cell_t offset) {
    if (offset < 0 || size_t(offset) >= datSize_) {
        return reportError(SP_ERROR_INSTRUCTION_PARAM);
    }
    return true;
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

bool
MethodVerifier::verifyStackAmount(cell_t amount) {
    // This is a rough estimate, we just make sure it definitely
    // won't go out of the heap.
    size_t estimate = size_t((amount < 0) ? -amount : amount);
    if (estimate >= heapSize_) {
        return reportError(SP_ERROR_INSTRUCTION_PARAM);
    }
    return true;
}

bool
MethodVerifier::verifyHeapAmount(cell_t amount) {
    // This is a rough estimate, we just make sure it definitely
    // won't go out of the heap.
    size_t estimate = size_t((amount < 0) ? -amount : amount);
    if (estimate >= heapSize_) {
        return reportError(SP_ERROR_INSTRUCTION_PARAM);
    }
    return true;
}

bool
MethodVerifier::verifyMemAmount(cell_t amount) {
    if (amount < 0 || size_t(amount) > memSize_) {
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
    if (!method_) {
        return reportError(SP_ERROR_FILE_FORMAT);
    }

    auto parser = rt_->image()->GetTypeParser(method_->signature);
    if (!parser.ReadFunctionSignatureArgCount(&arg_count_)) {
        return reportError(SP_ERROR_FILE_FORMAT);
    }

    if (!method_->locals)
        return true;

    parser = rt_->image()->GetTypeParser(method_->locals);

    uint16_t count;
    if (!parser.ReadLocalSlotCount(&count)) {
        return reportError(SP_ERROR_FILE_FORMAT);
    }

    local_sizes_ = ke::FixedArray<uint8_t>(count);
    for (uint16_t i = 0; i < count; i++) {
        uint8_t b;
        if (!parser.GetByte(&b))
            return false;

        if (b == cb::kConst) {
            parser.GetNextByte(&b);
            if (!parser.GetByte(&b))
                return false;
        }

        switch (b) {
            case cb::kBool:
            case cb::kInt32:
            case cb::kFloat32:
            case cb::kChar8:
            case cb::kAny:
            case cb::kTopFunction:
            case cb::kEnum:
                // Always int32.
                local_sizes_[i] = sizeof(cell_t);
                break;

            case cb::kFixedArray:
            case cb::kArray:
            case cb::kEnumStruct:
            case cb::kFunctionPtr:
            case cb::kTypeset:
                // Address-based but int32 for now.
                local_sizes_[i] = sizeof(cell_t);
                break;

            case cb::kInt64:
                local_sizes_[i] = sizeof(int64_t);
                break;

            default:
                return reportError(SP_ERROR_FILE_FORMAT);
        }

        parser.SkipNextType();
    }
    return true;
}

} // namespace sp::v2
