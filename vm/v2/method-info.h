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

#include <optional>

#include <amtl/am-fixedarray.h>
#include <amtl/am-refcounting.h>
#include <smx/smx-headers.h>
#include <sp_vm_types.h>
#include "base-method-info.h"
#include "control-flow.h"

namespace sp {
class CompiledFunction;
class SmxImage;
}
namespace sp::v2 {

class Runtime;

class MethodInfo final : public BaseMethodInfo
{
  public:
    MethodInfo(Runtime* rt, uint32_t method_index);
    ~MethodInfo();

    ke::RefPtr<ControlFlowGraph> BuildGraph() {
        InternalValidate();
        return graph_.take();
    }

    // For interpreter validation, we throw away the unused graph.
    bool Validate() {
        if (!checked_)
            InternalValidate();
        graph_ = nullptr;
        return *checked_;
    }

    uint32_t pcode_offset() const override;
    int32_t max_stack() const { return max_stack_; }
    uint32_t method_index() const { return method_index_; }
    uint32_t max_eval_stack_depth() const { return max_eval_stack_depth_; }
    uint32_t max_eval_stack_bytes() const { return max_eval_stack_bytes_; }
    const TypeDesc* GetTypeOfLocal(cell_t offset) const;
    const ke::FixedArray<const TypeDesc*>& local_types() const { return local_types_; }
    const ke::FixedArray<const TypeDesc*>& arg_types() const { return arg_types_; }
    ke::FixedArray<int32_t>& local_offsets() { return local_offsets_; }

    void setCompiledFunction(CompiledFunction* fun);
    CompiledFunction* jit() const override {
        return jit_.get();
    }

    void ClearCompilerCache() {
        local_types_ = {};
    }

  private:
    void InternalValidate();

  private:
    Runtime* rt_;
    uint32_t method_index_;
    std::unique_ptr<CompiledFunction> jit_;
    ke::RefPtr<ControlFlowGraph> graph_;

    std::optional<bool> checked_;
    int32_t max_stack_;
    uint32_t max_eval_stack_depth_;
    uint32_t max_eval_stack_bytes_;
    ke::FixedArray<const TypeDesc*> local_types_;
    ke::FixedArray<const TypeDesc*> arg_types_;
    ke::FixedArray<int32_t> local_offsets_;
};

} // namespace sp
