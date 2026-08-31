// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
//
#ifndef _INCLUDE_SOURCEPAWN_VM_LEGACY_METHOD_INFO_H_
#define _INCLUDE_SOURCEPAWN_VM_LEGACY_METHOD_INFO_H_

#include <amtl/am-refcounting.h>
#include <smx/smx-headers.h>
#include <sp_vm_types.h>
#include "base-method-info.h"
#include "control-flow.h"

namespace sp {
class CompiledFunction;
class SmxImage;
}
namespace sp::v1 {

class PluginRuntime;

class MethodInfo final : public BaseMethodInfo
{
  public:
    MethodInfo(PluginRuntime* rt, uint32_t codeOffset);
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
        return validation_error_ == SP_ERROR_NONE;
    }
    uint8_t local_size(unsigned index) { return local_sizes_[index]; }

    int validationError() const { return validation_error_; }
    uint32_t pcode_offset() const { return pcode_offset_; }
    uint32_t frame_id() const override { return pcode_offset_; }
    uint32_t TranslateInterpCip(const uint8_t* cip) const override;
    uint32_t TranslateJitCip(uint32_t cip) const override { return pcode_offset() + cip; }
    int32_t max_stack() const { return max_stack_; }

    const char* GetName() const override;
    const char* GetFilePath() const override;

    void setCompiledFunction(CompiledFunction* fun);
    CompiledFunction* jit() const override {
        return jit_.get();
    }

    // Note: these are only valid during interpreting or compilation.
    cell_t StackOffset(cell_t slot);
    // Returns the amount to change SP. It is always <= 0 since the stack
    // growns down.
    cell_t StackSizeForLocalSlots();

    void ClearCompilerCache() {
        local_offsets_ = {};
    }

  private:
    void InternalValidate();
    void BuildLocalOffsetTable();

  private:
    PluginRuntime* rt_;
    uint32_t pcode_offset_;
    std::unique_ptr<CompiledFunction> jit_;
    ke::RefPtr<ControlFlowGraph> graph_;

    bool checked_;
    int validation_error_;
    int32_t max_stack_;
    ke::FixedArray<uint8_t> local_sizes_;
    ke::FixedArray<cell_t> local_offsets_;
};

} // namespace sp

#endif // _INCLUDE_SOURCEPAWN_VM_LEGACY_METHOD_INFO_H_
