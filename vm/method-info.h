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
#ifndef _INCLUDE_SOURCEPAWN_VM_METHOD_INFO_H_
#define _INCLUDE_SOURCEPAWN_VM_METHOD_INFO_H_

#include <sp_vm_types.h>
#include <amtl/am-refcounting.h>
#include "control-flow.h"

namespace sp {

class PluginRuntime;
class CompiledFunction;

class MethodInfo final : public ke::Refcounted<MethodInfo>
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

  int validationError() const {
    return validation_error_;
  }
  uint32_t pcode_offset() const {
    return pcode_offset_;
  }
  int32_t max_stack() const {
    return max_stack_;
  }

  void setCompiledFunction(CompiledFunction* fun);
  CompiledFunction* jit() const {
    return jit_.get();
  }

 private:
  void InternalValidate();

 private:
  PluginRuntime* rt_;
  uint32_t pcode_offset_;
  std::unique_ptr<CompiledFunction> jit_;
  ke::RefPtr<ControlFlowGraph> graph_;

  bool checked_;
  int validation_error_;
  int32_t max_stack_;
};

} // namespace sp

#endif //_INCLUDE_SOURCEPAWN_VM_METHOD_INFO_H_
