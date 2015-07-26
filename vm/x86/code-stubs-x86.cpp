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
#include "x86-utils.h"
#include "jit_x86.h"
#include "environment.h"

using namespace sp;
using namespace SourcePawn;

#define __ masm.

bool
CodeStubs::InitializeFeatureDetection()
{
  MacroAssemblerX86 masm;
  MacroAssemblerX86::GenerateFeatureDetection(masm);
  CodeChunk code = LinkCode(env_, masm);
  if (!code.address())
    return false;
  MacroAssemblerX86::RunFeatureDetection(code.address());
  return true;
}

bool
CodeStubs::CompileInvokeStub()
{
  MacroAssemblerX86 masm;
  __ enterFrame(FrameType::Entry, 0);

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

  return_stub_ = reinterpret_cast<uint8_t *>(invoke_stub_.address()) + error.offset();
  return true;
}

CodeChunk
CodeStubs::CreateFakeNativeStub(SPVM_FAKENATIVE_FUNC callback, void *userData)
{
  AssemblerX86 masm;

  __ push(ebx);
  __ push(edi);
  __ push(esi);
  __ movl(edi, Operand(esp, 16)); // store ctx
  __ movl(esi, Operand(esp, 20)); // store params
  __ movl(ebx, esp);
  __ andl(esp, 0xfffffff0);
  __ subl(esp, 4);

  __ push(intptr_t(userData));
  __ push(esi);
  __ push(edi);
  __ call(ExternalAddress((void *)callback));
  __ movl(esp, ebx);
  __ pop(esi);
  __ pop(edi);
  __ pop(ebx);
  __ ret();

  return LinkCode(env_, masm);
}