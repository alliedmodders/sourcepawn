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
#include "plugin-runtime.h"
#include "plugin-context.h"
#include <assert.h>
#include <amtl/am-utility.h>
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

  auto& code = rt_->code();
  code_ = reinterpret_cast<const cell_t*>(code.bytes);
  stop_at_ = reinterpret_cast<const cell_t*>(code.bytes + code.length);
}

bool
MethodVerifier::verify()
{
  if (!IsAligned(startOffset_, sizeof(cell_t))) {
    reportError(SP_ERROR_INVALID_ADDRESS);
    return false;
  }

  method_ = code_ + (startOffset_ / sizeof(cell_t));
  cip_ = method_;

  {
    cell_t op;
    if (!readCell(&op))
      return false;
    if (op != OP_PROC) {
      reportError(SP_ERROR_INVALID_INSTRUCTION);
      return false;
    }
  }

  while (more()) {
    OPCODE op = (OPCODE)*cip_++;
    if (op == OP_PROC || op == OP_ENDPROC)
      break;

    if (!verifyOp(op))
      return false;
  }

  return true;
}

bool
MethodVerifier::verifyOp(OPCODE op)
{
  switch (op) {
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
    cell_t offset;
    if (!readCell(&offset))
      return false;
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
    cell_t offset;
    if (!readCell(&offset))
      return false;
    return verifyDatOffset(offset);
  }

  case OP_LODB_I:
  case OP_STRB_I:
  {
    cell_t val;
    if (!readCell(&val))
      return false;
    if (val != 1 && val != 2 && val != 4) {
      reportError(SP_ERROR_INVALID_INSTRUCTION);
      return false;
    }
    return true;
  }

  case OP_LIDX_B:
  case OP_IDXADDR_B:
  {
    cell_t val;
    if (!readCell(&val))
      return false;
    if (val != 1 && val != 2) {
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

    const cell_t* vec;
    return getCells(&vec, n);
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
      cell_t offset;
      if (!readCell(&offset) || !verifyDatOffset(offset))
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
      cell_t offset;
      if (!readCell(&offset) || !verifyStackOffset(offset))
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
      cell_t offset;
      if (!readCell(&offset) || !verifyStackOffset(offset))
        return false;
    }
    return true;
  }

  case OP_CALL:
  {
    cell_t offset;
    if (!readCell(&offset))
      return false;
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
  {
    cell_t offset;
    if (!readCell(&offset))
      return false;
    return verifyJumpOffset(offset);
  }

  case OP_SHL_C_PRI:
  case OP_SHL_C_ALT:
  case OP_SHR_C_PRI:
  case OP_SHR_C_ALT:
  case OP_ADD_C:
  case OP_SMUL_C:
  case OP_EQ_C_PRI:
  case OP_EQ_C_ALT:
  case OP_HALT:
  {
    cell_t val;
    return readCell(&val);
  }

  case OP_MOVS:
  {
    cell_t val;
    if (!readCell(&val))
      return false;
    return verifyMemAmount(val);
  }

  case OP_FILL:
  {
    cell_t val;
    if (!readCell(&val))
      return false;
    return verifyMemAmount(val);
  }

  // Note - STACK and HEAP are verified at runtime.
  case OP_STACK:
  case OP_HEAP:
  case OP_CONST_PRI:
  case OP_CONST_ALT:
  case OP_BOUNDS:
  {
    cell_t value;
    return readCell(&value);
  }

  case OP_SYSREQ_C:
  {
    cell_t index;
    if (!readCell(&index))
      return false;
    if (index < 0 || size_t(index) >= rt_->image()->NumNatives()) {
      reportError(SP_ERROR_INSTRUCTION_PARAM);
      return false;
    }
    return true;
  }

  case OP_SYSREQ_N:
  {
    cell_t index;
    if (!readCell(&index))
      return false;
    if (index < 0 || size_t(index) >= rt_->image()->NumNatives()) {
      reportError(SP_ERROR_INSTRUCTION_PARAM);
      return false;
    }
    cell_t nparams;
    if (!readCell(&nparams))
      return false;
    return verifyParamCount(nparams);
  }

  case OP_LOAD_BOTH:
  {
    cell_t offs1, offs2;
    if (!readCell(&offs1) ||
        !readCell(&offs2) ||
        !verifyDatOffset(offs1) ||
        !verifyDatOffset(offs2))
    {
      return false;
    }
    return true;
  }

  case OP_LOAD_S_BOTH:
  {
    cell_t offs1, offs2;
    if (!readCell(&offs1) ||
        !readCell(&offs2) ||
        !verifyStackOffset(offs1) ||
        !verifyStackOffset(offs2))
    {
      return false;
    }
    return true;
  }

  case OP_CONST:
  {
    cell_t offset, value;
    if (!readCell(&offset) || !readCell(&value))
      return false;
    return verifyDatOffset(offset);
  }

  case OP_CONST_S:
  {
    cell_t offset, value;
    if (!readCell(&offset) || !readCell(&value))
      return false;
    return verifyStackOffset(offset);
  }

  case OP_TRACKER_PUSH_C:
  {
    cell_t amount;
    if (!readCell(&amount))
      return false;
    return verifyHeapAmount(amount);
  }

  case OP_GENARRAY:
  case OP_GENARRAY_Z:
  {
    cell_t ndims;
    if (!readCell(&ndims))
      return false;
    return verifyParamCount(ndims);
  }

  case OP_SWITCH:
  {
    cell_t tableOffset;
    if (!readCell(&tableOffset) || !verifyJumpOffset(tableOffset))
      return false;

    const cell_t* casetbl = code_ + (tableOffset / sizeof(cell_t));
    assert(casetbl >= code_ && casetbl < stop_at_);

    cell_t ncases, defaultOffset;
    {
      ke::SaveAndSet<const cell_t*> saved_pos(&cip_, casetbl);

      cell_t op;
      if (!readCell(&op))
      if (op != OP_CASETBL) {
        reportError(SP_ERROR_INVALID_INSTRUCTION);
        return false;
      }

      if (!readCell(&ncases))
        return false;
      if (!readCell(&defaultOffset) || !verifyJumpOffset(defaultOffset))
        return false;
      if (ncases >= INT_MAX || ncases < 0) {
        reportError(SP_ERROR_INVALID_INSTRUCTION);
        return false;
      }

      for (cell_t i = 0; i < ncases; i++) {
        cell_t value, offset;
        if (!readCell(&value) ||
            !readCell(&offset) ||
            !verifyJumpOffset(offset))
        {
          return true;
        }
      }
    }
    return true;
  }

  case OP_CASETBL:
  {
    cell_t ncases;
    if (!readCell(&ncases))
      return false;

    const cell_t* vec;
    if (!getCells(&vec, (ncases * 2) + 1))
      return false;

    // Nothing to do here. This is handled in OP_SWITCH.
    return true;
  }

  default:
    // Should have been caught earlier.
    assert(op != OP_PROC);
    reportError(SP_ERROR_INVALID_INSTRUCTION);
    return false;
  }
}

bool
MethodVerifier::readCell(cell_t* out)
{
  if (cip_ >= stop_at_) {
    reportError(SP_ERROR_INVALID_INSTRUCTION);
    return false;
  }
  *out = *cip_++;
  return true;
}

bool
MethodVerifier::getCells(const cell_t** out, size_t ncells)
{
  if (cip_ + ncells > stop_at_) {
    reportError(SP_ERROR_INVALID_INSTRUCTION);
    return false;
  }
  *out = cip_;
  cip_ += ncells;
  return true;
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
MethodVerifier::verifyJumpOffset(cell_t offset)
{
  if (offset < 0 || !IsAligned(offset, sizeof(cell_t))) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }

  // We do not allow jumping outside the method, or to the start of the method.
  // Note we don't know the actual endpoint of the method yet :(
  const cell_t* target = code_ + (offset / sizeof(cell_t));
  if (target <= method_ || target >= stop_at_) {
    reportError(SP_ERROR_INSTRUCTION_PARAM);
    return false;
  }
  return true;
}

bool
MethodVerifier::verifyParamCount(cell_t nparams)
{
  // This is a rough estimate, we just make sure it definitely
  // won't go out of the heap.
  if (nparams < 0 || size_t(nparams) >= heapSize_) {
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
