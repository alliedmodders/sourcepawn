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
#ifndef _include_sourcepawn_vm_method_verifier_h_
#define _include_sourcepawn_vm_method_verifier_h_

#include <functional>
#include <vector>

#include <sp_vm_types.h>
#include <smx/smx-v1-opcodes.h>
#include "control-flow.h"

namespace sp {

class PluginRuntime;

class MethodVerifier final
{
 public:
  explicit MethodVerifier(PluginRuntime* rt, uint32_t startOffset);

  typedef std::function<void(cell_t)> ExternalFuncRefCallback;
  void collectExternalFuncRefs(const ExternalFuncRefCallback& callback);

  ke::RefPtr<ControlFlowGraph> verify();

  int32_t max_stack() const {
    return max_stack_;
  }
  int error() const {
    return error_;
  }

 private:
  bool more() const {
    return cip_ < stop_at_;
  }

 private:
  bool verifyOp(OPCODE op);
  bool verifyStackOffset(cell_t offset);
  bool verifyDatOffset(cell_t offset);
  bool verifyJumpOffset(cell_t offset);
  bool verifyParamCount(cell_t nparams);
  bool verifyDimensionCount(cell_t ndims);
  bool verifyStackAmount(cell_t amount);
  bool verifyHeapAmount(cell_t amount);
  bool verifyMemAmount(cell_t amount);
  bool verifyCallOffset(cell_t offset);
  void reportError(int err);
  cell_t readCell();

  struct VerifyData : public IBlockData {
    VerifyData()
     : stack_balance(0)
    {}
    uint32_t stack_balance;
    std::vector<int32_t> heap_balance;
  };

  bool handleJoins();
  bool verifyJoin(Block* block, VerifyData* a, VerifyData* b);
  bool verifyJoins(Block* block);
  bool pushStack(uint32_t num_cells);
  bool popStack(uint32_t num_cells);
  bool pushHeap(uint32_t num_cells);
  bool popHeap(uint32_t num_cells);

 private:
  PluginRuntime* rt_;
  ke::RefPtr<ControlFlowGraph> graph_;
  Block* block_;
  std::vector<Block*> verify_joins_;
  uint32_t code_features_;
  uint32_t startOffset_;
  size_t memSize_;
  size_t datSize_;
  size_t heapSize_;
  uint32_t max_stack_;
  const cell_t* code_;
  const cell_t* method_;
  const cell_t* insn_;
  const cell_t* cip_;
  const cell_t* prev_cip_;
  const cell_t* stop_at_;
  ExternalFuncRefCallback collect_func_refs_;
  int error_;
};

} // namespace sp

#endif // _include_sourcepawn_vm_method_verifier_h_
