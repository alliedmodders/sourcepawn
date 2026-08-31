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
#include "graph-builder.h"
#include "v2/method-verifier.h"
#include "v2/opcodes.h"
#include "v2/plugin-runtime.h"

namespace sp::v2 {

using namespace ke;

MethodVerifier::MethodVerifier(PluginRuntime* rt, uint32_t startOffset)
 : rt_(rt),
   block_(nullptr),
   startOffset_(startOffset),
   memSize_(rt_->HeapSize()),
   datSize_(rt_->image()->DescribeData().length),
   heapSize_(memSize_ - datSize_),
   max_stack_(0),
   code_(nullptr),
   cip_(nullptr),
   prev_cip_(nullptr),
   stop_at_(nullptr),
   error_(SP_ERROR_NONE)
{
    assert(datSize_ < memSize_);
    assert(heapSize_ <= memSize_ - datSize_);

    code_version_ = rt_->image()->DescribeCode().version;
    code_features_ = rt_->image()->DescribeCode().features;

    auto& code = rt_->code();
    code_ = reinterpret_cast<const cell_t*>(code.bytes);
    stop_at_ = reinterpret_cast<const cell_t*>(code.bytes + code.length);
}

ke::RefPtr<ControlFlowGraph>
MethodVerifier::verify() {
    if (!IsAligned(startOffset_, sizeof(cell_t))) {
        reportError(SP_ERROR_INVALID_ADDRESS);
        return nullptr;
    }

    auto image = rt_->image();
    if (image->HasRtti()) {
        method_ = image->GetMethodRttiByOffset(startOffset_);
        if (!method_ || method_->pcode_start != startOffset_) {
            reportError(SP_ERROR_INVALID_ADDRESS);
            return nullptr;
        }
    }

    if (!verifyLocalSlots())
        return nullptr;

    GraphBuilder gb(rt_, startOffset_);
    graph_ = gb.build();
    if (!graph_) {
        reportError(gb.error_code());
        return nullptr;
    }

    AutoClearBlockData<VerifyData> acbd(graph_);

    for (auto iter = graph_->rpoBegin(); iter != graph_->rpoEnd(); iter++) {
        block_ = *iter;
        if (!handleJoins())
            return nullptr;

        prev_cip_ = nullptr;

        cip_ = reinterpret_cast<const cell_t*>(block_->start());
        while (cip_ < reinterpret_cast<const cell_t*>(block_->end())) {
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
    max_stack_ *= sizeof(cell_t);

    return graph_;
}

bool
MethodVerifier::verifyOp(OPCODE op) {
    switch (op) {
        case OP_PROC:
        case OP_NOP:
        case OP_BREAK:
        case OP_LOAD_I:
        case OP_STOR_I:
        case OP_IDXADDR:
        case OP_MOVE_PRI:
        case OP_MOVE_ALT:
        case OP_XCHG:
        case OP_SHL:
        case OP_SHR:
        case OP_SSHR:
        case OP_SMUL:
        case OP_SDIV:
        case OP_SDIV_ALT:
        case OP_ADD:
        case OP_SUB_ALT:
        case OP_AND:
        case OP_OR:
        case OP_XOR:
        case OP_NOT:
        case OP_NEG:
        case OP_INVERT:
        case OP_ZERO_PRI:
        case OP_ZERO_ALT:
        case OP_EQ:
        case OP_NEQ:
        case OP_SLESS:
        case OP_SLEQ:
        case OP_SGRTR:
        case OP_SGEQ:
        case OP_INC_PRI:
        case OP_INC_ALT:
        case OP_DEC_PRI:
        case OP_DEC_ALT:
        case OP_STRADJUST_PRI:
        case OP_MOVE_I64:
        case OP_TRUNCATE_I64:
        case OP_TEST_I64:
        case OP_SLESS_I64:
        case OP_SLEQ_I64:
        case OP_SGRTR_I64:
        case OP_SGEQ_I64:
        case OP_EQ_I64:
        case OP_NEQ_I64:
        case OP_TEST_F32:
        case OP_NEG_F32:
        case OP_MUL_F32:
        case OP_DIV_ALT_F32:
        case OP_ADD_F32:
        case OP_SUB_ALT_F32:
        case OP_EQ_F32:
        case OP_NEQ_F32:
        case OP_LESS_F32:
        case OP_LEQ_F32:
        case OP_GRTR_F32:
        case OP_GEQ_F32:
        case OP_CVT_F32:
        case OP_MOD_ALT_F32:
        case OP_SDIV_ALT_I32:
        case OP_SMOD_ALT_I32:
            return true;

        case OP_SWAP_PRI:
        case OP_SWAP_ALT:
            // Simulate the swap operation.
            if (!popStack(1))
                return false;
            return pushStack(1);

        case OP_PUSH_PRI:
        case OP_PUSH_ALT:
            return pushStack(1);

        case OP_POP_PRI:
        case OP_POP_ALT:
            return popStack(1);

        case OP_ADDR_ALT:
        case OP_ADDR_PRI:
        {
            cell_t offset = readCell();
            return verifyStackOffset(offset, 0);
        }

        case OP_LOAD_S_PRI:
        case OP_LOAD_S_ALT:
        case OP_LREF_S_PRI:
        case OP_LREF_S_ALT:
        case OP_SREF_S_PRI:
        case OP_SREF_S_ALT:
        case OP_STOR_S_ALT:
        case OP_STOR_S_PRI:
        case OP_ZERO_S:
        {
            cell_t offset = readCell();
            return verifyStackOffset(offset, sizeof(cell_t));
        }

        case OP_ZERO_S_I64:
        {
            cell_t offset = readCell();
            return verifyStackOffset(offset, sizeof(int64_t));
        }

        case OP_STOR_S_C: {
            cell_t offset = readCell();
            readCell();
            return verifyStackOffset(offset, sizeof(cell_t));
        }

        case OP_INVERT_I64:
        case OP_CVT_I64:
        case OP_NEG_I64:
        case OP_SMUL_I64:
        case OP_ADD_I64:
        case OP_SUB_ALT_I64:
        case OP_SHL_I64:
        case OP_SSHR_I64:
        case OP_SHR_I64:
        case OP_OR_I64:
        case OP_AND_I64:
        case OP_XOR_I64:
        case OP_SDIV_ALT_I64:
        case OP_SMOD_ALT_I64:
        case OP_STOR_S_PRI_I64:
        {
            cell_t offset = readCell();
            return verifyStackOffset(offset, sizeof(int64_t));
        }

        case OP_STOR_S_C_I64: {
            cell_t offset = readCell();
            readCell();
            readCell();
            return verifyStackOffset(offset, sizeof(int64_t));
        }

        case OP_LOAD_PRI:
        case OP_LOAD_ALT:
        case OP_STOR_PRI:
        case OP_STOR_ALT: {
            cell_t offset = readCell();
            return verifyDatOffset(offset);
        }

        case OP_LODB_I:
        case OP_STRB_I: {
            cell_t val = readCell();
            if (val != 1 && val != 2 && val != 4) {
                reportError(SP_ERROR_INVALID_INSTRUCTION);
                return false;
            }
            return true;
        }

        case OP_PUSH_C: {
            readCell();
            return pushStack(1);
        }

        case OP_PUSH_S:
        {
            cell_t offset = readCell();
            if (!verifyStackOffset(offset, sizeof(cell_t)))
                return false;
            return pushStack(1);
        }

        case OP_PUSH_ADR:
        {
            cell_t offset = readCell();
            if (!verifyStackOffset(offset, 0))
                return false;
            return pushStack(1);
        }

        case OP_PUSH_I_I64:
            return pushStack(2);

        case OP_CALL: {
            // An OP_CALL must be preceded by a PUSH_C variant, and it must be in the
            // same block.
            if (!prev_cip_ || *prev_cip_ != OP_PUSH_C) {
                reportError(SP_ERROR_INVALID_INSTRUCTION);
                return false;
            }
            cell_t nparams = prev_cip_[1];
            cell_t offset = readCell();
            if (!verifyCallOffset(offset))
                return false;
            if (!popStack(nparams + 1))
                return false;
            if (collect_func_refs_)
                collect_func_refs_(offset);
            return true;
        }

        case OP_JUMP:
        case OP_JZER:
        case OP_JNZ:
        case OP_JEQ:
        case OP_JNEQ:
        case OP_JSLESS:
        case OP_JSLEQ:
        case OP_JSGRTR:
        case OP_JSGEQ:
        case OP_SHL_C_PRI:
        case OP_SHL_C_ALT:
        case OP_ADD_C:
        case OP_SMUL_C:
        case OP_CONST_PRI:
        case OP_CONST_ALT:
        case OP_BOUNDS:
        case OP_SWITCH: {
            cip_++;
            return true;
        }

        case OP_MOVS: {
            cell_t val = readCell();
            return verifyMemAmount(val);
        }

        case OP_FILL: {
            cell_t val = readCell();
            return verifyMemAmount(val);
        }

        // Note - STACK and HEAP are verified at runtime.
        case OP_HEAP:
        {
            cell_t value = readCell();
            if (!ke::IsAligned(value, sizeof(cell_t))) {
                reportError(SP_ERROR_INSTRUCTION_PARAM);
                return false;
            }
            if (value <= 0) {
                reportError(SP_ERROR_INSTRUCTION_PARAM);
                return false;
            }
            if (value > INT_MAX / 4) {
                reportError(SP_ERROR_INSTRUCTION_PARAM);
                return false;
            }
            return true;
        }

        case OP_SYSREQ_N: {
            cell_t index = readCell();
            if (index < 0 || size_t(index) >= rt_->image()->NumNatives()) {
                reportError(SP_ERROR_INSTRUCTION_PARAM);
                return false;
            }
            cell_t nparams = readCell();
            if (!pushStack(1))
                return false;
            if (!popStack(nparams + 1))
                return false;
            return verifyParamCount(nparams);
        }

        case OP_GENARRAY:
        case OP_GENARRAY_Z: {
            cell_t ndims = readCell();
            if (!verifyDimensionCount(ndims))
                return false;
            if (!popStack(ndims - 1))
                return false;
            return true;
        }

        case OP_INITARRAY_PRI:
        case OP_INITARRAY_ALT: {
            constexpr cell_t kMaxCells = INT_MAX / (2 * (int)sizeof(cell_t));

            cell_t addr = readCell();
            cell_t iv_size = readCell();
            cell_t data_copy_size = readCell();
            cell_t data_fill_size = readCell();
            cell_t fill_value = readCell();
            if (iv_size < 0 || data_copy_size < 0 || data_fill_size < 0 || iv_size >= kMaxCells ||
                data_copy_size >= kMaxCells || data_fill_size >= kMaxCells ||
                (!data_fill_size && fill_value) || !ke::IsAligned(addr, sizeof(cell_t))) {
                reportError(SP_ERROR_INSTRUCTION_PARAM);
                return false;
            }

            cell_t copy_addr = addr + iv_size * sizeof(cell_t);
            cell_t fill_addr = copy_addr + data_copy_size * sizeof(cell_t);
            if (copy_addr < addr || fill_addr < copy_addr ||
                !ke::IsUintAddSafe<uint32_t>(fill_addr, data_fill_size * sizeof(cell_t))) {
                reportError(SP_ERROR_INSTRUCTION_PARAM);
                return false;
            }

            // If there's nothing to read from DAT, we can early return.
            if (!iv_size && !data_copy_size)
                return true;

            cell_t end_addr = addr + (iv_size + data_copy_size) * sizeof(cell_t);
            if (!verifyDatOffset(addr) || !verifyDatOffset(end_addr - 1))
                return false;
            return true;
        }

        case OP_CASETBL:
            cip_ = insn_ + GetCaseTableSize(reinterpret_cast<const uint8_t*>(insn_));
            return true;

        case OP_HEAP_SAVE:
            block_->data<VerifyData>()->heap_scope_depth++;
            return true;

        case OP_HEAP_RESTORE:
            if (!block_->data<VerifyData>()->heap_scope_depth) {
                reportError(SP_ERROR_INVALID_INSTRUCTION);
                return false;
            }
            block_->data<VerifyData>()->heap_scope_depth--;
            return true;

        case OP_RETN:
            block_->heap_scope_depth() = block_->data<VerifyData>()->heap_scope_depth;
            return true;

        default:
            // Should have been caught earlier.
            assert(op != OP_PROC);
            reportError(SP_ERROR_INVALID_INSTRUCTION);
            return false;
    }
}

cell_t
MethodVerifier::readCell() {
    assert(cip_ < stop_at_);
    return *cip_++;
}

bool
MethodVerifier::verifyJoin(VerifyData* first, VerifyData* other) {
    if (first->stack_balance != other->stack_balance) {
        reportError(SP_ERROR_INSTRUCTION_PARAM);
        return false;
    }

    if (first->heap_scope_depth != other->heap_scope_depth) {
        reportError(SP_ERROR_INSTRUCTION_PARAM);
        return false;
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
    // be an illegal backedge to the entry block. While this is not allowed
    // currently, because of OP_PROC, it may be allowed in the future, so we
    // handle it.
    if (!found_pred) {
        assert(verify_later);
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
MethodVerifier::pushStack(uint32_t num_cells) {
    VerifyData* v = block_->data<VerifyData>();
    if (!ke::IsUint32AddSafe(v->stack_balance, num_cells)) {
        reportError(SP_ERROR_INSTRUCTION_PARAM);
        return false;
    }

    v->stack_balance += num_cells;
    if (v->stack_balance > INT_MAX / sizeof(cell_t)) {
        reportError(SP_ERROR_INSTRUCTION_PARAM);
        return false;
    }

    if (v->stack_balance > max_stack_)
        max_stack_ = v->stack_balance;
    return true;
}

bool
MethodVerifier::popStack(uint32_t num_cells) {
    VerifyData* v = block_->data<VerifyData>();
    if (num_cells > v->stack_balance) {
        reportError(SP_ERROR_INSTRUCTION_PARAM);
        return false;
    }

    v->stack_balance -= num_cells;
    return true;
}

bool MethodVerifier::verifyStackOffset(cell_t offset, uint32_t op_size) {
    // Modern stack is typed.
    if (offset < 0) {
        uint32_t arg_slot = -offset - 1;
        if (arg_slot >= arg_count_) {
            reportError(SP_ERROR_INSTRUCTION_PARAM);
            return false;
        }
    } else {
        if (offset >= local_sizes_.size()) {
            reportError(SP_ERROR_INSTRUCTION_PARAM);
            return false;
        }
        if (op_size && local_sizes_[offset] != op_size) {
            reportError(SP_ERROR_INSTRUCTION_PARAM);
            return false;
        }
    }
    return true;
}

bool
MethodVerifier::verifyDatOffset(cell_t offset) {
    if (offset < 0 || size_t(offset) >= datSize_) {
        reportError(SP_ERROR_INSTRUCTION_PARAM);
        return false;
    }
    return true;
}

bool
MethodVerifier::verifyDimensionCount(cell_t ndims) {
    if (ndims <= 0) {
        reportError(SP_ERROR_INSTRUCTION_PARAM);
        return false;
    }
    return true;
}

bool
MethodVerifier::verifyParamCount(cell_t nparams) {
    if (nparams < 0 || nparams > SP_MAX_CALL_ARGUMENTS) {
        reportError(SP_ERROR_INSTRUCTION_PARAM);
        return false;
    }
    return true;
}

bool
MethodVerifier::verifyCallOffset(cell_t offset) {
    if (offset < 0 || !IsAligned(offset, sizeof(cell_t))) {
        reportError(SP_ERROR_INSTRUCTION_PARAM);
        return false;
    }

    const cell_t* target = code_ + (offset / sizeof(cell_t));
    if (target < code_ || target >= stop_at_) {
        reportError(SP_ERROR_INSTRUCTION_PARAM);
        return false;
    }
    if (target[0] != OP_PROC) {
        reportError(SP_ERROR_INSTRUCTION_PARAM);
        return false;
    }
    return true;
}

bool
MethodVerifier::verifyStackAmount(cell_t amount) {
    // This is a rough estimate, we just make sure it definitely
    // won't go out of the heap.
    size_t estimate = size_t((amount < 0) ? -amount : amount);
    if (estimate >= heapSize_) {
        reportError(SP_ERROR_INSTRUCTION_PARAM);
        return false;
    }
    return true;
}

bool
MethodVerifier::verifyHeapAmount(cell_t amount) {
    // This is a rough estimate, we just make sure it definitely
    // won't go out of the heap.
    size_t estimate = size_t((amount < 0) ? -amount : amount);
    if (estimate >= heapSize_) {
        reportError(SP_ERROR_INSTRUCTION_PARAM);
        return false;
    }
    return true;
}

bool
MethodVerifier::verifyMemAmount(cell_t amount) {
    if (amount < 0 || size_t(amount) > memSize_) {
        reportError(SP_ERROR_INSTRUCTION_PARAM);
        return false;
    }
    return true;
}

void
MethodVerifier::collectExternalFuncRefs(const ExternalFuncRefCallback& callback) {
    collect_func_refs_ = callback;
}

void
MethodVerifier::reportError(int err) {
    // Break here to find why verification failed.
    error_ = err;
}

bool MethodVerifier::verifyLocalSlots() {
    if (!method_) {
        reportError(SP_ERROR_FILE_FORMAT);
        return false;
    }

    auto parser = rt_->image()->GetTypeParser(method_->signature);
    if (!parser.ReadFunctionSignatureArgCount(&arg_count_)) {
        reportError(SP_ERROR_FILE_FORMAT);
        return false;
    }

    if (!method_->locals)
        return true;

    parser = rt_->image()->GetTypeParser(method_->locals);

    uint16_t count;
    if (!parser.ReadLocalSlotCount(&count)) {
        reportError(SP_ERROR_FILE_FORMAT);
        return false;
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
                reportError(SP_ERROR_FILE_FORMAT);
                return false;
        }

        max_stack_ += local_sizes_[i];

        parser.SkipNextType();
    }
    return true;
}

} // namespace sp::v2
