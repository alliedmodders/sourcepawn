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
#include "method-verifier.h"
#include "graph-builder.h"
#include "opcodes.h"
#include "plugin-runtime.h"
#include "plugin-context.h"
#include <assert.h>
#include <limits.h>

namespace sp {

using namespace ke;

MethodVerifier::MethodVerifier(PluginRuntime* rt, uint32_t startOffset)
 : rt_(rt),
   startOffset_(startOffset),
   memSize_(rt_->context()->HeapSize()),
   datSize_(rt_->image()->DescribeData().length),
   heapSize_(memSize_ - datSize_),
   code_(nullptr),
   cip_(nullptr),
   stop_at_(nullptr),
   error_(SP_ERROR_NONE)
{
  assert(datSize_ < memSize_);
  assert(heapSize_ <= memSize_ - datSize_);

  code_features_ = rt_->image()->DescribeCode().features;

  auto& code = rt_->code();
  code_ = reinterpret_cast<const cell_t*>(code.bytes);
  stop_at_ = reinterpret_cast<const cell_t*>(code.bytes + code.length);
}

ke::RefPtr<ControlFlowGraph>
MethodVerifier::verify()
{
  if (!IsAligned(startOffset_, sizeof(cell_t))) {
    reportError(SP_ERROR_INVALID_ADDRESS);
    return nullptr;
  }

  GraphBuilder gb(rt_, startOffset_);
  graph_ = gb.build();
  if (!graph_) {
    reportError(gb.error_code());
    return nullptr;
  }

  method_ = code_ + (startOffset_ / sizeof(cell_t));

  for (auto iter = graph_->rpoBegin(); iter != graph_->rpoEnd(); iter++) {
    Block* block = *iter;
    cip_ = reinterpret_cast<const cell_t*>(block->start());
    while (cip_ < reinterpret_cast<const cell_t*>(block->end())) {
      OPCODE op = (OPCODE)*cip_++;
      if (!verifyOp(op))
        return nullptr;
    }
  }
  return graph_;
}

bool
MethodVerifier::verifyOp(OPCODE op)
{
  switch (op) {
  case OP_PROC:
  case OP_NOP:
  case OP_BREAK:
  case OP_LOAD_I:
  case OP_STOR_I:
  case OP_LIDX:
  case OP_IDXADDR:
  case OP_MOVE_PRI:
  case OP_MOVE_ALT:
  case OP_XCHG:
  case OP_RETN:
  case OP_SHL:
  case OP_SHR:
  case OP_SSHR:
  case OP_SMUL:
  case OP_SDIV:
  case OP_SDIV_ALT:
  case OP_ADD:
  case OP_SUB:
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
  case OP_INC_I:
  case OP_DEC_PRI:
  case OP_DEC_ALT:
  case OP_DEC_I:
  case OP_TRACKER_POP_SETHEAP:
  case OP_STRADJUST_PRI:
  case OP_PUSH_PRI:
  case OP_PUSH_ALT:
  case OP_POP_PRI:
  case OP_POP_ALT:
  case OP_SWAP_PRI:
  case OP_SWAP_ALT:
    return true;

  case OP_ADDR_ALT:
  case OP_ADDR_PRI:
  case OP_LOAD_S_PRI:
  case OP_LOAD_S_ALT:
  case OP_LREF_S_PRI:
  case OP_LREF_S_ALT:
  case OP_SREF_S_PRI:
  case OP_SREF_S_ALT:
  case OP_STOR_S_ALT:
  case OP_STOR_S_PRI:
  case OP_INC_S:
  case OP_DEC_S:
  case OP_ZERO_S:
  {
    cell_t offset = readCell();
    return verifyStackOffset(offset);
  }

  case OP_LOAD_PRI:
  case OP_LOAD_ALT:
  case OP_STOR_PRI:
  case OP_STOR_ALT:
  case OP_INC:
  case OP_DEC:
  case OP_ZERO:
  {
    cell_t offset = readCell();
    return verifyDatOffset(offset);
  }

  case OP_LODB_I:
  case OP_STRB_I:
  {
    cell_t val = readCell();
    if (val != 1 && val != 2 && val != 4) {
      reportError(SP_ERROR_INVALID_INSTRUCTION);
      return false;
    }
    return true;
  }

  case OP_PUSH_C:
  case OP_PUSH2_C:
  case OP_PUSH3_C:
  case OP_PUSH4_C:
  case OP_PUSH5_C:
  {
    size_t n = 1;
    if (op >= OP_PUSH2_C)
      n = ((op - OP_PUSH2_C) / 4) + 2;

    cip_ += n;
    return true;
  }

  case OP_PUSH:
  case OP_PUSH2:
  case OP_PUSH3:
  case OP_PUSH4:
  case OP_PUSH5:
  {
    size_t n = 1;
    if (op >= OP_PUSH2)
      n = ((op - OP_PUSH2) / 4) + 2;
    
    for (size_t i = 0; i < n; i++) {
      cell_t offset = readCell();
      if (!verifyDatOffset(offset))
        return false;
    }
    return true;
  }

  case OP_PUSH_S:
  case OP_PUSH2_S:
  case OP_PUSH3_S:
  case OP_PUSH4_S:
  case OP_PUSH5_S:
  {
    size_t n = 1;
    if (op >= OP_PUSH2_S)
      n = ((op - OP_PUSH2_S) / 4) + 2;

    for (size_t i = 0; i < n; i++) {
      cell_t offset = readCell();
      if (!verifyStackOffset(offset))
        return false;
    }
    return true;
  }

  case OP_PUSH_ADR:
  case OP_PUSH2_ADR:
  case OP_PUSH3_ADR:
  case OP_PUSH4_ADR:
  case OP_PUSH5_ADR:
  {
    size_t n = 1;
    if (op >= OP_PUSH2_ADR)
      n = ((op - OP_PUSH2_ADR) / 4) + 2;

    for (size_t i = 0; i < n; i++) {
      cell_t offset = readCell();
      if (!verifyStackOffset(offset))
        return false;
    }
    return true;
  }

  case OP_CALL:
  {
    cell_t offset = readCell();
    if (!verifyCallOffset(offset))
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
  case OP_EQ_C_PRI:
  case OP_EQ_C_ALT:
  case OP_CONST_PRI:
  case OP_CONST_ALT:
  case OP_BOUNDS:
  case OP_SWITCH:
  case OP_TRACKER_PUSH_C:
  {
    cip_++;
    return true;
  }

  case OP_HALT:
  {
    reportError(SP_ERROR_INVALID_INSTRUCTION);
    return false;
  }

  case OP_MOVS:
  {
    cell_t val = readCell();
    return verifyMemAmount(val);
  }

  case OP_FILL:
  {
    cell_t val = readCell();
    return verifyMemAmount(val);
  }

  // Note - STACK and HEAP are verified at runtime.
  case OP_STACK:
  case OP_HEAP:
  {
    cell_t value = readCell();
    if (!ke::IsAligned(value, sizeof(cell_t))) {
      reportError(SP_ERROR_INSTRUCTION_PARAM);
      return false;
    }
    return true;
  }

  case OP_SYSREQ_C:
  {
    cell_t index = readCell();
    if (index < 0 || size_t(index) >= rt_->image()->NumNatives()) {
      reportError(SP_ERROR_INSTRUCTION_PARAM);
      return false;
    }
    return true;
  }

  case OP_SYSREQ_N:
  {
    cell_t index = readCell();
    if (index < 0 || size_t(index) >= rt_->image()->NumNatives()) {
      reportError(SP_ERROR_INSTRUCTION_PARAM);
      return false;
    }
    cell_t nparams = readCell();
    return verifyParamCount(nparams);
  }

  case OP_LOAD_BOTH:
  {
    cell_t offs1 = readCell();
    cell_t offs2 = readCell();
    if (!verifyDatOffset(offs1) ||
        !verifyDatOffset(offs2))
    {
      return false;
    }
    return true;
  }

  case OP_LOAD_S_BOTH:
  {
    cell_t offs1 = readCell();
    cell_t offs2 = readCell();
    if (!verifyStackOffset(offs1) ||
        !verifyStackOffset(offs2))
    {
      return false;
    }
    return true;
  }

  case OP_CONST:
  {
    cell_t offset = readCell();
    cip_++;
    return verifyDatOffset(offset);
  }

  case OP_CONST_S:
  {
    cell_t offset = readCell();
    cip_++;
    return verifyStackOffset(offset);
  }

  case OP_GENARRAY:
  case OP_GENARRAY_Z:
  {
    cell_t ndims = readCell();
    return verifyDimensionCount(ndims);
  }

  case OP_REBASE:
  {
    if (!(code_features_ & SmxConsts::kCodeFeatureDirectArrays)) {
      reportError(SP_ERROR_INVALID_INSTRUCTION);
      return false;
    }

    cell_t addr = readCell();
    cell_t iv_size = readCell();
    cell_t data_size = readCell();
    if (iv_size <= 0 ||
        data_size <= 0 ||
        iv_size >= INT_MAX / 2 ||
        data_size >= INT_MAX / 2 ||
        !ke::IsUintAddSafe<uint32_t>(addr, iv_size + data_size) ||
        !ke::IsAligned(addr, sizeof(cell_t)) ||
        !ke::IsAligned(iv_size, sizeof(cell_t)) ||
        !ke::IsAligned(data_size, sizeof(cell_t)))
    {
      reportError(SP_ERROR_INSTRUCTION_PARAM);
      return false;
    }
    if (!verifyDatOffset(addr) || !verifyDatOffset(addr + iv_size + data_size - 1))
      return false;
    return true;
  }

  case OP_CASETBL:
    cip_ = insn_ + GetCaseTableSize(reinterpret_cast<const uint8_t*>(insn_));
    return true;

  default:
    // Should have been caught earlier.
    assert(op != OP_PROC);
    reportError(SP_ERROR_INVALID_INSTRUCTION);
    return false;
  }
}

cell_t
MethodVerifier::readCell()
{
  assert(cip_ < stop_at_);
  return *cip_++;
}

bool
MethodVerifier::verifyStackOffset(cell_t offset)
{
  // This is a rough estimate, we just make sure it definitely
  // won't go out of the heap.
  size_t estimate = size_t((offset < 0) ? -offset : offset);
  if (estimate >= heapSize_) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }
  return true;
}

bool
MethodVerifier::verifyDatOffset(cell_t offset)
{
  if (offset < 0 || size_t(offset) >= datSize_) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }
  return true;
}

bool
MethodVerifier::verifyDimensionCount(cell_t ndims)
{
  if (ndims <= 0 || ndims > sDIMEN_MAX) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }
  return true;
}

bool
MethodVerifier::verifyParamCount(cell_t nparams)
{
  if (nparams < 0 || nparams > SP_MAX_CALL_ARGUMENTS) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }
  return true;
}

bool
MethodVerifier::verifyCallOffset(cell_t offset)
{
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
MethodVerifier::verifyStackAmount(cell_t amount)
{
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
MethodVerifier::verifyHeapAmount(cell_t amount)
{
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
MethodVerifier::verifyMemAmount(cell_t amount)
{
  if (amount < 0 || size_t(amount) > memSize_) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }
  return true;
}

void
MethodVerifier::collectExternalFuncRefs(const ExternalFuncRefCallback& callback)
{
  collect_func_refs_ = callback;
}

void
MethodVerifier::reportError(int err)
{
  // Break here to find why verification failed.
  error_ = err;
}

} // namespace sp
