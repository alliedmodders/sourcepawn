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
#include <sp_vm_api.h>
#include "code-stubs.h"
#include "linking.h"
#include "macro-assembler-x64.h"
#include "constants-x64.h"
#include "plugin-context.h"

#define __ masm.

namespace sp {

bool
CodeStubs::InitializeFeatureDetection()
{
  return true;
}

#if 0
bool
CodeStubs::CompileInvokeStub()
{
  MacroAssembler masm;
  __ enterFrame(JitFrameType::Entry, 0);

  __ push(rbx);
  __ push(r12);
  __ push(r13);
  __ push(r14);
  __ push(r15);

  // We push 5 values, plus 2 for the frame size.
  static const intptr_t kFpOffsetToPreAlignedSp = -(5 + kExtraWordsInSpFrame) * 8;

  // arg0 = cx
  // arg1 = code
  // arg2 = rval
  
  // Save the context and rval pointers.
  const Register context = saved1;
  const Register rvalptr = saved0;
  __ movq(context, ArgReg0);
  __ movq(rvalptr, ArgReg2);
  
  // Set up runtime registers.
  __ movq(dat, Operand(context, static_cast<int32_t>(PluginContext::offsetOfMemory())));
  __ movq(stk, Operand(context, static_cast<int32_t>(PluginContext::offsetOfSp())));
  __ addq(stk, dat);

  // Align the stack.
  __ andq(rsp, 0xfffffff0);

  // Call into plugin.
  __ call(ArgReg1);

  // Store the rval.
  __ movq(Operand(rvalptr, 0), pri);

  // Store latest stk. If we have an error code, we'll jump directly to here,
  // so rax will already be set.
  Label ret;
  __ bind(&ret);
  __ subq(stk, dat);
  __ movq(Operand(context, static_cast<int32_t>(PluginContext::offsetOfSp())), stk);

  // Restore registers and leave.
  __ leaq(rsp, Operand(rbp, kFpOffsetToPreAlignedSp));
  __ pop(r15);
  __ pop(r14);
  __ pop(r13);
  __ pop(r12);
  __ pop(rbx);
  __ leaveFrame();
  __ ret();

  // The universal emergency return will jump to here.
  Label error;
  __ bind(&error);
  __ jmp(&ret);

  invoke_stub_ = LinkCode(env_, masm);
  if (!invoke_stub_.address())
    return false;

  return_stub_ = reinterpret_cast<uint8_t*>(invoke_stub_.address()) + error.offset();
  return true;
}
#endif

} // namespace sp
