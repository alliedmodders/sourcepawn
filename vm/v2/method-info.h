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

#include <memory>
#include <optional>

#include <amtl/am-fixedarray.h>
#include <amtl/am-refcounting.h>
#include <smx/smx-headers.h>
#include <sp_vm_types.h>
#include <utils/bitset.h>
#include "base-method-info.h"
#include "control-flow.h"

namespace sp {
class CompiledFunction;
class SmxImage;
}
namespace sp::v2 {

class LLCode;

class Runtime;

class MethodInfo final : public BaseMethodInfo
{
  public:
    MethodInfo(v2::Runtime* rt, uint32_t method_index, const TypeDesc* signature);
    ~MethodInfo() override;

    ke::RefPtr<ControlFlowGraph> BuildGraph() {
        InternalValidate();
        return graph_.take();
    }

    bool Validate() {
        if (!checked_)
            InternalValidate();
        return *checked_;
    }

    uint32_t pcode_offset() const;
    uint32_t frame_id() const override { return method_index_; }
    uint32_t TranslateInterpCip(const uint8_t* cip) const override;
    uint32_t TranslateJitCip(uint32_t cip) const override;
    int32_t max_stack() const { return max_stack_; }
    uint32_t method_index() const { return method_index_; }
    uint32_t max_eval_stack_depth() const { return max_eval_stack_depth_; }
    const TypeDesc* signature() const { return signature_; }
    bool IsLegacyVariadic() const {
        return signature_->args().size() > 0 && signature_->args().back()->IsLegacyVarArgs();
    }
    uint32_t FormalArgc() const {
        return signature_->args().size() - (IsLegacyVariadic() ? 1 : 0);
    }
    uint32_t max_eval_stack_bytes() const { return max_eval_stack_bytes_; }
    const TypeDesc* GetTypeOfLocal(cell_t offset) const;
    const ke::FixedArray<const TypeDesc*>& local_types() const { return local_types_; }
    const ke::FixedArray<const TypeDesc*>& arg_types() const { return arg_types_; }
    ke::FixedArray<int32_t>& local_offsets() { return local_offsets_; }

    void SetMutatedArgs(BitSet&& mutated_args) { mutated_args_ = std::move(mutated_args); }
    const BitSet& mutated_args() const { return mutated_args_; }

    void setCompiledFunction(CompiledFunction* fun);
    CompiledFunction* jit() const override {
        return jit_.get();
    }

    void set_llcode(std::unique_ptr<LLCode> code);
    LLCode* llcode() const {
        return llcode_.get();
    }

    const char* GetName() const override;
    const char* GetFilePath() const override;

    void ClearCompilerCache() {
        local_types_ = {};
    }

  private:
    void InternalValidate();

  private:
    Runtime* rt_;
    uint32_t method_index_;
    std::unique_ptr<CompiledFunction> jit_;
    std::unique_ptr<LLCode> llcode_;
    ke::RefPtr<ControlFlowGraph> graph_;

    std::optional<bool> checked_;
    int32_t max_stack_;
    uint32_t max_eval_stack_depth_;
    uint32_t max_eval_stack_bytes_;
    ke::FixedArray<const TypeDesc*> local_types_;
    ke::FixedArray<const TypeDesc*> arg_types_;
    ke::FixedArray<int32_t> local_offsets_;
    BitSet mutated_args_;
    const TypeDesc* signature_;
};

} // namespace sp
