// vim: set sts=2 ts=8 sw=2 tw=99 et:
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
#pragma once

#include <sp_vm_types.h>
#include <functional>
#include <vector>

#include <amtl/am-fixedarray.h>
#include <smx/smx-v2-opcodes.h>
#include <sp_vm_types.h>
#include "control-flow.h"
#include "type-desc.h"

namespace sp {
namespace v2 {

class Runtime;

class MethodVerifier final
{
  public:
    explicit MethodVerifier(Runtime* rt, uint32_t method_index);

    typedef std::function<void(uint32_t)> ExternalFuncRefCallback;
    void collectExternalFuncRefs(const ExternalFuncRefCallback& callback);

    ke::RefPtr<ControlFlowGraph> verify();

    int32_t max_stack() const { return max_stack_; }
    uint32_t max_eval_stack_depth() const { return max_eval_stack_depth_; }
    uint32_t max_eval_stack_bytes() const { return max_eval_stack_bytes_; }
    ke::FixedArray<const TypeDesc*>&& local_types() { return std::move(local_types_); }
    ke::FixedArray<const TypeDesc*>&& arg_types() { return std::move(arg_types_); }

    const TypeDesc* cell_type() const;
    const TypeDesc* any_type() const;
    const TypeDesc* int64_type() const;
    const TypeDesc* float32_type() const;

  private:
    bool more() const {
        return cip_ < stop_at_;
    }

  private:
    bool verifyOp(OPCODE op);
    const TypeDesc* verifyStackOffset(cell_t offset);
    bool verifyDatAddress(cell_t offset);
    bool verifyDatString(uint16_t index);
    bool verifyArrayType(const TypeDesc* type) {
        if (!type->IsArrayish())
            return false;
        if (type->array_elt()->element_size() > UINT16_MAX)
            return false;
        return true;
    }
    const TypeDesc* verifyGlobalIndex(uint16_t index);
    bool verifyJumpOffset(cell_t offset);
    bool verifyParamCount(cell_t nparams);
    bool verifyDimensionCount(cell_t ndims);
    bool verifyCallIndex(uint32_t method_index);
    bool verifyCallArguments(const smx_rtti_method* method, uint32_t arg_count);
    bool reportError(int err);

    cell_t readCell() {
        return read<cell_t>();
    }
    int16_t readInt16() {
        return read<int16_t>();
    }
    template <typename T> T read() {
        assert(cip_ + sizeof(T) <= stop_at_);
        T val = *reinterpret_cast<const T*>(cip_);
        cip_ += sizeof(T);
        return val;
    }

    struct VerifyData : public IBlockData {
        VerifyData()
         : stack_bytes(0)
        {}
        VerifyData(const VerifyData& other)
         : stack(other.stack),
           stack_bytes(other.stack_bytes)
        {}

        VerifyData& operator=(const VerifyData& other) {
            stack = other.stack;
            stack_bytes = other.stack_bytes;
            return *this;
        }

        std::vector<const TypeDesc*> stack;
        uint32_t stack_bytes;

        std::unique_ptr<VerifyData> entry;
    };

    bool handleJoins();
    bool mergeTracker(Block* block, VerifyData* other);
    bool verifyJoin(VerifyData* first, VerifyData* other);
    bool verifyJoins(Block* block);
    bool pushStack(const TypeDesc* type);
    bool popStack(TypeKind kind);
    bool popStack(const TypeDesc** type);
    bool popCell();
    bool checkCell(const TypeDesc* td);
    bool popIntOrFloat();
    bool checkIntOrFloat(const TypeDesc* td);
    bool popInt32();
    bool popStack(uint32_t num_operands);
    bool pushHeap(uint32_t num_cells);

    bool verifyLocalSlots();

    bool ValidateStore(const TypeDesc* dest, const TypeDesc* src);

  private:
    Runtime* rt_;
    SmxImage* smx_;
    ke::RefPtr<ControlFlowGraph> graph_;
    Block* block_;
    const smx_rtti_method* method_ = nullptr;
    ke::FixedArray<const TypeDesc*> local_types_;
    ke::FixedArray<const TypeDesc*> arg_types_;
    std::vector<Block*> verify_joins_;
    const TypeDesc* return_type_ = nullptr;
    uint32_t arg_count_ = 0;
    int code_version_;
    uint32_t code_features_;
    uint32_t method_index_;
    size_t datSize_;
    uint32_t max_stack_;
    uint32_t max_eval_stack_depth_ = 0;
    uint32_t max_eval_stack_bytes_ = 0;
    const uint8_t* code_;
    const uint8_t* insn_;
    const uint8_t* cip_;
    const uint8_t* prev_cip_;
    const uint8_t* stop_at_;
    ExternalFuncRefCallback collect_func_refs_;
};

} // namespace v2
} // namespace sp
