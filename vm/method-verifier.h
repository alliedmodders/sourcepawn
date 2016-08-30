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

#include <sp_vm_types.h>
#include <smx/smx-v1-opcodes.h>
#include <amtl/am-function.h>

namespace sp {

class PluginRuntime;

class MethodVerifier final
{
 public:
  explicit MethodVerifier(PluginRuntime* rt, uint32_t startOffset);

  typedef ke::Lambda<void(cell_t)> ExternalFuncRefCallback;
  void collectExternalFuncRefs(const ExternalFuncRefCallback& callback);

  bool verify();

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
  bool readCell(cell_t* out);
  bool getCells(const cell_t** out, size_t ncells);
  void reportError(int err);

 private:
  PluginRuntime* rt_;
  uint32_t startOffset_;
  size_t memSize_;
  size_t datSize_;
  size_t heapSize_;
  const cell_t* code_;
  const cell_t* method_;
  const cell_t* cip_;
  const cell_t* stop_at_;
  ExternalFuncRefCallback collect_func_refs_;
  int error_;
};

} // namespace sp

#endif // _include_sourcepawn_vm_method_verifier_h_
