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
#include "jit_x86.h"
#include "environment.h"

using namespace sp;
using namespace SourcePawn;

#define __ masm.

bool
CodeStubs::InitializeFeatureDetection()
{
  MacroAssembler masm;
  MacroAssembler::GenerateFeatureDetection(masm);
  CodeChunk code = LinkCode(env_, masm);
  if (!code.address())
    return false;
  MacroAssembler::RunFeatureDetection(code.address());
  return true;
}


bool
CodeStubs::CompileInvokeStub()
{
  MacroAssembler masm;
  __ enterFrame(JitFrameType::Entry, 0);

  __ push(esi);
  __ push(edi);
  __ push(ebx);

  static const intptr_t kContextOffset = 8 + 0 * sizeof(intptr_t);
  static const intptr_t kCodeOffset = 8 + 1 * sizeof(intptr_t);
  static const intptr_t kRvalOffset = 8 + 2 * sizeof(intptr_t);
  static const intptr_t kFpOffsetToPreAlignedSp = -20;

  // ebx = cx
  __ movl(ebx, Operand(ebp, kContextOffset));

  // ecx = code
  __ movl(ecx, Operand(ebp, kCodeOffset));

  // eax = cx->memory
  __ movl(eax, Operand(ebx, PluginContext::offsetOfMemory()));

  // Set up run-time registers.
  __ movl(edi, Operand(ebx, PluginContext::offsetOfSp()));
  __ addl(edi, eax);
  __ movl(esi, eax);
  __ movl(ebx, edi);

  // Align the stack.
  __ andl(esp, 0xfffffff0);

  // Call into plugin.
  __ call(ecx);

  // Store the rval.
  __ movl(ecx, Operand(ebp, kRvalOffset));
  __ movl(Operand(ecx, 0), pri);

  // Store latest stk. If we have an error code, we'll jump directly to here,
  // so eax will already be set.
  Label ret;
  __ bind(&ret);
  __ subl(stk, dat);
  __ movl(ecx, Operand(ebp, kContextOffset));
  __ movl(Operand(ecx, PluginContext::offsetOfSp()), stk);

  // Restore stack.
  __ lea(esp, Operand(ebp, kFpOffsetToPreAlignedSp));

  // Restore registers and gtfo.
  __ pop(ebx);
  __ pop(edi);
  __ pop(esi);
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
