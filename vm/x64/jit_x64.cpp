// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
// This file is part of SourcePawn.
// 
// SourcePawn is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// SourcePawn is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with SourcePawn.  If not, see <http://www.gnu.org/licenses/>.
#include "jit_x64.h"
#include "constants-x64.h"
#include "plugin-context.h"
#include "environment.h"
#include "code-stubs.h"

namespace sp {

#define __ masm.

Compiler::Compiler(PluginRuntime *rt, cell_t pcode_offs)
 : CompilerBase(rt, pcode_offs)
{
}

Compiler::~Compiler()
{
}

case OP_CONST_PRI:
  {
    Register reg = (op == OP_CONST_PRI) ? pri : alt;
    cell_t val = readCell();
    __ movl(reg, val);
    break;
  }

  case OP_PROC:
  {
    __ enterFrame(FrameType::Scripted, pcode_start_);

    // Push the old frame onto the stack.
    __ movq(scratch0, AddressValue(context_));
    __ movl(scratch1, Operand(scratch0, PluginContext::offsetOfFrm()));
    __ movl(Operand(stk, -4), scratch1);
    __ subq(stk, 8); // extra unused slot for non-existent CIP

    // Get and store the new frame.
    __ movq(frm, stk);
    __ movq(scratch1, stk);
    __ subq(scratch1, dat);
    __ movl(Operand(scratch0, PluginContext::offsetOfFrm()), scratch1);
    break;
  }

  case OP_RETN:
  {
    // Restore the old frame pointer.
    __ movl(frm, Operand(stk, 4)); // get the old frm
    __ addq(stk, 8);               // pop stk
    __ movl(AddressOperand(context_->addressOfFrm()), frm);
    __ addq(frm, dat);

    // Remove parameters.
    __ movl(scratch0, Operand(stk, 0));
    __ leaq(stk, Operand(stk, scratch0, ScaleFour, 4));

    __ leaveFrame();
    __ ret();
    break;
  }

  case OP_CALL:
    if (!emitCall())
      return false;
    break;

  case OP_PUSH_ALT:
  {
    Register reg = (op == OP_PUSH_PRI) ? pri : alt;
    __ movl(Operand(stk, -4), reg);
    __ subq(stk, 4);
    break;
  }

  case OP_PUSH_ADR:
  case OP_PUSH2_ADR:
  case OP_PUSH3_ADR:
  case OP_PUSH4_ADR:
  case OP_PUSH5_ADR:
  {
    int n = 1;
    if (op >= OP_PUSH2_ADR)
      n = ((op - OP_PUSH2_ADR) / 4) + 2;

    int i = 1;

    // We temporarily relocate FRM to be a local address instead of an
    // absolute address.
    __ subq(frm, dat);
    do {
      cell_t offset = readCell();
      __ movl(scratch0, frm);
      __ addl(scratch0, offset);
      __ movl(Operand(stk, -(4 * i)), scratch0);
    } while (i++ < n);
    __ subq(stk, 4 * n);
    __ addq(frm, dat);
    break;
  }

  case OP_PUSH_C:
  {
    int n = 1;
    if (op >= OP_PUSH2_C)
      n = ((op - OP_PUSH2_C) / 4) + 2;

    int i = 1;
    do {
      cell_t val = readCell();
      __ movl(Operand(stk, -(4 * i)), val);
    } while (i++ < n);
    __ subq(stk, 4 * n);
    break;
  }

  case OP_PUSH_S:
  case OP_PUSH2_S:
  case OP_PUSH3_S:
  case OP_PUSH4_S:
  case OP_PUSH5_S:
  {
    int n = 1;
    if (op >= OP_PUSH2_S)
      n = ((op - OP_PUSH2_S) / 4) + 2;

    int i = 1;
    do {
      cell_t offset = readCell();
      __ movl(scratch0, Operand(frm, offset));
      __ movl(Operand(stk, -(4 * i)), scratch0);
    } while (i++ < n);
    __ subq(stk, 4 * n);
    break;
  }

  case OP_SYSREQ_N:
    if (!emitSysreqN())
      return false;
    break;

  case OP_STACK:
  {
    cell_t amount = readCell();
    __ addq(stk, amount);

    if (amount > 0) {
      if (amount >= context_->HeapSize()) {
        error_ = SP_ERROR_INVALID_INSTRUCTION;
        return false;
      }

      // Check if the stack went beyond the stack top - usually a compiler error.
      __ movq(scratch0, stk);
      __ subq(scratch0, dat);
      __ cmpl(scratch0, context_->HeapSize());
     jumpOnError(not_below, SP_ERROR_STACKMIN);
    } else {
      if (-amount >= context_->HeapSize()) {
        error_ = SP_ERROR_INVALID_INSTRUCTION;
        return false;
      }

      // Check if the stack is going to collide with the heap.
      __ movl(scratch0, AddressOperand(context_->addressOfHp()));
      __ leaq(scratch1, Operand(dat, scratch0, NoScale, STACK_MARGIN));
      __ cmpq(stk, scratch1);
      jumpOnError(below, SP_ERROR_STACKLOW);
    }
    break;
  }

  case OP_HEAP:
  {
    cell_t amount = readCell();
    __ movq(scratch1, AddressValue(context_));
    __ movl(alt, Operand(scratch1, PluginContext::offsetOfHp()));
    __ movl(scratch0, alt);
    __ addl(scratch0, amount);

    if (amount < 0) {
      __ cmpl(scratch0, context_->DataSize());
      jumpOnError(below, SP_ERROR_HEAPMIN);
    } else {
      __ leaq(scratch2, Operand(dat, scratch0, NoScale, STACK_MARGIN));
      __ cmpq(scratch2, stk);
      jumpOnError(above, SP_ERROR_HEAPLOW);
    }

    __ movl(Operand(scratch1, PluginContext::offsetOfHp()), scratch0);
    break;
  }

  case OP_STOR_I:
    emitCheckAddress(alt);
    __ movl(Operand(dat, alt, NoScale), pri);
    break;

  case OP_LREF_S_PRI:
  {
    Register reg = (op == OP_LREF_S_PRI) ? pri : alt;
    cell_t offset = readCell();
    __ movl(reg, Operand(frm, offset));
    __ movl(reg, Operand(dat, reg, NoScale));
    break;
  }

  case OP_ZERO_PRI:
    __ xorq(pri, pri);
    break;

  case OP_NONE:
  case OP_LOAD_PRI:
  case OP_LOAD_ALT:
  case OP_LOAD_S_PRI:
  case OP_LOAD_S_ALT:
  case OP_LREF_S_ALT:
  case OP_LOAD_I:
  case OP_LODB_I:
  case OP_CONST_ALT:
  case OP_ADDR_PRI:
  case OP_ADDR_ALT:
  case OP_STOR_PRI:
  case OP_STOR_ALT:
  case OP_STOR_S_PRI:
  case OP_STOR_S_ALT:
  case OP_SREF_S_PRI:
  case OP_SREF_S_ALT:
  case OP_STRB_I:
  case OP_LIDX:
  case OP_LIDX_B:
  case OP_IDXADDR:
  case OP_IDXADDR_B:
  case OP_MOVE_PRI:
  case OP_MOVE_ALT:
  case OP_XCHG:
  case OP_PUSH_PRI:
  case OP_PUSH:
  case OP_POP_PRI:
  case OP_POP_ALT:
  case OP_JUMP:
  case OP_JZER:
  case OP_JNZ:
  case OP_JEQ:
  case OP_JNEQ:
  case OP_JSLESS:
  case OP_JSLEQ:
  case OP_JSGRTR:
  case OP_JSGEQ:
  case OP_SHL:
  case OP_SHR:
  case OP_SSHR:
  case OP_SHL_C_PRI:
  case OP_SHL_C_ALT:
  case OP_SHR_C_PRI:
  case OP_SHR_C_ALT:
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
  case OP_ADD_C:
  case OP_SMUL_C:
  case OP_ZERO_ALT:
  case OP_ZERO:
  case OP_ZERO_S:
  case OP_EQ:
  case OP_NEQ:
  case OP_SLESS:
  case OP_SLEQ:
  case OP_SGRTR:
  case OP_SGEQ:
  case OP_EQ_C_PRI:
  case OP_EQ_C_ALT:
  case OP_INC_PRI:
  case OP_INC_ALT:
  case OP_INC:
  case OP_INC_S:
  case OP_INC_I:
  case OP_DEC_PRI:
  case OP_DEC_ALT:
  case OP_DEC:
  case OP_DEC_S:
  case OP_DEC_I:
  case OP_MOVS:
  case OP_FILL:
  case OP_HALT:
  case OP_BOUNDS:
  case OP_SYSREQ_C:
  case OP_SWITCH:
  case OP_CASETBL:
  case OP_SWAP_PRI:
  case OP_SWAP_ALT:
  case OP_PUSH2_C:
  case OP_PUSH2:
  case OP_PUSH3_C:
  case OP_PUSH3:
  case OP_PUSH4_C:
  case OP_PUSH4:
  case OP_PUSH5_C:
  case OP_PUSH5:
  case OP_LOAD_BOTH:
  case OP_LOAD_S_BOTH:
  case OP_CONST:
  case OP_CONST_S:
  case OP_UNGEB_SYSREQ_ND:
  case OP_TRACKER_PUSH_C:
  case OP_TRACKER_POP_SETHEAP:
  case OP_GENARRAY:
  case OP_GENARRAY_Z:
  case OP_STRADJUST_PRI:
  case OP_ENDPROC:
  case OP_FABS:
  case OP_FLOAT:
  case OP_FLOATADD:
  case OP_FLOATSUB:
  case OP_FLOATMUL:
  case OP_FLOATDIV:
  case OP_RND_TO_NEAREST:
  case OP_RND_TO_FLOOR:
  case OP_RND_TO_CEIL:
  case OP_RND_TO_ZERO:
  case OP_FLOATCMP:
  case OP_FLOAT_GT:
  case OP_FLOAT_GE:
  case OP_FLOAT_LT:
  case OP_FLOAT_LE:
  case OP_FLOAT_NE:
  case OP_FLOAT_EQ:
  case OP_FLOAT_NOT:
  default:
    assert(false);
    error_ = SP_ERROR_INVALID_INSTRUCTION;
    return false;
  }
  return true;
}

bool
Compiler::emitSysreqN()
{
  uint32_t native_index = readCell();

  if (native_index >= image_->NumNatives()) {
    error_ = SP_ERROR_INSTRUCTION_PARAM;
    return false;
  }

  NativeEntry* native = rt_->NativeAt(native_index);
  uint32_t nparams = readCell();

  if (native->status == SP_NATIVE_BOUND &&
      !(native->flags & (SP_NTVFLAG_EPHEMERAL|SP_NTVFLAG_OPTIONAL)))
  {
    uint32_t replacement = rt_->GetNativeReplacement(native_index);
    assert(replacement == OP_NOP);
  }

  // Store the number of parameters on the stack.
  __ movl(Operand(stk, -4), nparams);
  __ subq(stk, 4);
  if (!emitLegacyNativeCall(native_index, native))
    return false;
  __ addq(stk, (nparams + 1) * sizeof(cell_t));

  return true;
}

bool
Compiler::emitCall()
{
  cell_t offset = readCell();

  // If this offset looks crappy, i.e. not aligned or out of bounds, we just
  // abort.
  if (offset % 4 != 0 || uint32_t(offset) >= rt_->code().length) {
    error_ = SP_ERROR_INSTRUCTION_PARAM;
    return false;
  }

  CompiledFunction *fun = rt_->GetJittedFunctionByOffset(offset);
  if (!fun) {
    // Need to emit a delayed thunk.
    CallThunk thunk(offset);
    __ movq(scratch0, &thunk.target, &thunk.patch);
    __ callWithABI(scratch0);
    if (!call_thunks_.append(thunk))
      return false;
  } else {
    // Function is already emitted, we can do a direct call.
    __ callWithABI(AddressValue(fun->GetEntryAddress()));
  }

  // Map the return address to the cip that started this call.
  emitCipMapping(op_cip_);
  return true;
}

bool
Compiler::emitLegacyNativeCall(uint32_t native_index, NativeEntry* native)
{
  CodeLabel return_offset;
  __ enterInlineExitFrame(ExitFrameType::Native, native_index, &return_offset);

  // Check whether the native is bound.
  bool immutable = native->status == SP_NATIVE_BOUND &&
                   !(native->flags & (SP_NTVFLAG_EPHEMERAL|SP_NTVFLAG_OPTIONAL));
  if (!immutable) {
    __ movq(r11, AddressOperand(&native->legacy_fn));
    __ testq(r11, r11);
    jumpOnError(zero, SP_ERROR_INVALID_NATIVE);
  }

  assert(intptr_t(context_) == intptr_t(rt_->GetBaseContext()));
  assert(r10 != ArgReg0 && r10 != ArgReg1);
  assert(r11 != ArgReg0 && r11 != ArgReg1);

  // Save alt.
  __ push(alt);

  // Save the old heap pointer. Note we put context into ArgReg0.
  __ movq(ArgReg0, AddressValue(context_));
  __ movl(r10, Operand(ArgReg0, PluginContext::offsetOfHp()));
  __ push(r10);

  // Set the |params| parameter.
  __ movq(ArgReg1, stk);

  // Relocate stk to be dat-relative, and update the context.
  __ subq(stk, dat);
  __ movl(Operand(ArgReg0, PluginContext::offsetOfSp()), stk);

  // Invoke the native.
  if (immutable)
    __ movq(r11, AddressValue(reinterpret_cast<void*>(native->legacy_fn)));
  __ callWithABI(r11);
  __ bind(&return_offset);
  emitCipMapping(op_cip_); // Map the return address to the cip that initiated this call.

  // Restore the heap pointer.
  __ pop(r10);
  __ movl(AddressOperand(context_->addressOfHp()), r10);

  // Restore alt.
  __ pop(alt);

  // Restore SP.
  __ addq(stk, dat);

  // Note: no ret, the frame is inline. We add 8 to rsp instead.
  __ leaveExitFrame();
  __ addq(rsp, 8);

  // Check for errors. Note we jump directly to the return stub since the error
  // has already been reported.
  __ cmpl(AddressOperand(env_->addressOfExceptionCode()), 0);
  __ j(not_equal, &return_reported_error_);
  return true;
}

void
Compiler::jumpOnError(ConditionCode cc, int err)
{
  // Note: we accept 0 for err. In this case we expect the error to be in eax.
  ErrorPath path(op_cip_, err);
  __ j(cc, &path.label);

  error_paths_.append(path);
}

void
Compiler::emitCallThunks()
{
  for (size_t i = 0; i < call_thunks_.length(); i++) {
    CallThunk& thunk = call_thunks_[i];

    Label error;
    __ bind(&thunk.target);

    // Enter the exit frame. This aligns the stack.
    __ enterExitFrame(ExitFrameType::Helper, 0);

    // Reserve some aligned space.
    __ subq(rsp, 16);

    // Set arguments.
    __ movq(ArgReg3, &thunk.patch);
    __ leaq(ArgReg2, Operand(rsp, 0));
    __ movl(ArgReg1, thunk.pcode_offset);
    __ movq(ArgReg0, AddressValue(rt_));

    __ callWithABI(AddressValue(reinterpret_cast<void *>(CompileFromThunk)));
    __ pop(scratch0);
    __ leaveExitFrame();

    __ testl(rax, rax);
    jumpOnError(not_zero);

    __ jmp(scratch0);
  }
}

void
Compiler::emitErrorHandlers()
{
  Label return_to_invoke;

  if (report_error_.used()) {
    __ bind(&report_error_);

    // Create the exit frame. We always get here through a call from the
    // opcode (and always via an out-of-line thunk).
    __ enterExitFrame(ExitFrameType::Helper, 0);

    // Align the stack and call.
    __ movl(ArgReg0, rax);
    __ callWithABI(AddressValue(reinterpret_cast<void*>(InvokeReportError)));
    __ leaveExitFrame();
    __ jmp(&return_to_invoke);
  }

  // The timeout uses a special stub.
  if (throw_timeout_.used()) {
    __ bind(&throw_timeout_);
    __ breakpoint();

    // Create the exit frame.
    __ enterExitFrame(ExitFrameType::Helper, 0);

    // Since the return stub wipes out the stack, we don't need to addl after
    // the call.
    __ callWithABI(AddressValue(reinterpret_cast<void *>(InvokeReportTimeout)));
    __ leaveExitFrame();
    __ jmp(&return_reported_error_);
  }

  if (return_reported_error_.used()) {
    __ bind(&return_reported_error_);
    __ call(&return_to_invoke);
  }

  if (return_to_invoke.used()) {
    __ bind(&return_to_invoke);

    // We get here either through an explicit call, or a call that terminated
    // in a tail-jmp here.
    __ enterExitFrame(ExitFrameType::Helper, 0);

    // We cannot jump to the return stub just yet. We could be multiple frames
    // deep, and our |rbp| does not match the initial frame. Find and restore
    // it now.
    __ callWithABI(AddressValue(reinterpret_cast<void*>(find_entry_fp)));
    __ leaveExitFrame();

    __ movq(rbp, rax);
    __ jmp(AddressValue(reinterpret_cast<void*>(env_->stubs()->ReturnStub())));
  }
}

void
Compiler::emitCheckAddress(Register reg)
{
  // Check if we're in memory bounds.
  __ cmpl(reg, context_->HeapSize());
  jumpOnError(not_below, SP_ERROR_MEMACCESS);

  // Check if we're in the invalid region between hp and sp.
  Label done;
  __ movl(scratch0, AddressOperand(context_->addressOfHp()));
  __ cmpl(reg, scratch0);
  __ j(below, &done);
  __ leaq(scratch1, Operand(dat, reg, NoScale));
  __ cmpq(scratch1, stk);
  jumpOnError(below, SP_ERROR_MEMACCESS);
  __ bind(&done);
}

void
Compiler::emitThrowPath(int err)
{
  __ movl(rax, err);
  __ jmp(&report_error_);
}

void
CompilerBase::PatchCallThunk(uint8_t* pc, void* target)
{
  *reinterpret_cast<void **>(pc - 8) = target;
}

} // namespace sp
