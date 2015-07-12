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
#include "code-stubs-x86.h"

using namespace sp;
using namespace SourcePawn;

#define __ masm.

bool
CodeStubs::InitializeFeatureDetection()
{
  MacroAssemblerX86 masm;
  MacroAssemblerX86::GenerateFeatureDetection(masm);
  void *code = LinkCode(env_, masm);
  if (!code)
    return false;
  MacroAssemblerX86::RunFeatureDetection(code);
  return true;
}

bool
CodeStubs::CompileInvokeStub()
{
  AssemblerX86 masm;

  __ push(ebp);
  __ movl(ebp, esp);

  __ push(esi);   // ebp - 4
  __ push(edi);   // ebp - 8
  __ push(ebx);   // ebp - 12
  __ push(esp);   // ebp - 16

  // ebx = cx
  __ movl(ebx, Operand(ebp, 8 + 4 * 0));

  // ecx = code
  __ movl(ecx, Operand(ebp, 8 + 4 * 1));

  // eax = cx->memory
  __ movl(eax, Operand(ebx, PluginContext::offsetOfMemory()));

  // Set up run-time registers.
  __ movl(edi, Operand(ebx, PluginContext::offsetOfSp()));
  __ addl(edi, eax);
  __ movl(esi, eax);
  __ movl(ebx, edi);

  // Align the stack.
  __ andl(esp, 0xfffffff0);

  // Set up the last piece of the invoke frame. This lets us find the bounds
  // of the call stack.
  __ movl(eax, intptr_t(Environment::get()));
  __ movl(eax, Operand(eax, Environment::offsetOfTopFrame()));
  __ movl(Operand(eax, InvokeFrame::offsetOfEntrySp()), esp);

  // Call into plugin (align the stack first).
  __ call(ecx);

  // Get input context, store rval.
  __ movl(ecx, Operand(ebp, 8 + 4 * 2));
  __ movl(Operand(ecx, 0), pri);

  // Set no error.
  __ movl(eax, SP_ERROR_NONE);

  // Store latest stk. If we have an error code, we'll jump directly to here,
  // so eax will already be set.
  Label ret;
  __ bind(&ret);
  __ subl(stk, dat);
  __ movl(ecx, Operand(ebp, 8 + 4 * 0));
  __ movl(Operand(ecx, PluginContext::offsetOfSp()), stk);

  // Restore stack.
  __ movl(esp, Operand(ebp, -16));

  // Restore registers and gtfo.
  __ pop(ebx);
  __ pop(edi);
  __ pop(esi);
  __ pop(ebp);
  __ ret();

  // The universal emergency return will jump to here.
  Label error;
  __ bind(&error);
  __ movl(ecx, Operand(ebp, 8 + 4 * 0)); // ret-path expects ecx = ctx
  __ jmp(&ret);

  invoke_stub_ = LinkCode(env_, masm);
  if (!invoke_stub_)
    return false;

  return_stub_ = reinterpret_cast<uint8_t *>(invoke_stub_) + error.offset();
  return true;
}

SPVM_NATIVE_FUNC
CodeStubs::CreateFakeNativeStub(SPVM_FAKENATIVE_FUNC callback, void *pData)
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

  __ push(intptr_t(pData));
  __ push(esi);
  __ push(edi);
  __ call(ExternalAddress((void *)callback));
  __ movl(esp, ebx);
  __ pop(esi);
  __ pop(edi);
  __ pop(ebx);
  __ ret();

  return (SPVM_NATIVE_FUNC)LinkCode(env_, masm);
}

class TypeSpecIter
{
 public:
  TypeSpecIter(const NativeSpec* spec)
   : iter_(spec->signature),
     end_(iter_ + spec->length),
     done_(false),
     by_ref(false),
     key(uint8_t(TypeSpec::none))
  {
    next();
  }

  TypeSpecIter nextCopy() {
    TypeSpecIter copy(*this);
    copy.next();
    return copy;
  }
  void next() {
    assert(!done());

    if (iter_ >= end_) {
      done_ = true;
      if (!ensureMore())
        return;
    }

    if (*iter_ == uint8_t(TypeSpec::byref)) {
      by_ref = true;
      iter_++;
    }

    key = *iter_++;
  }
  bool done() const {
    return done_;
  }

 private:
   bool ensureMore() {
     if (iter_ >= end_) {
       done_ = true;
       return false;
     }
     return true;
   }

 private:
  const uint8_t* iter_;
  const uint8_t* end_;
  bool done_;

 public:
  bool by_ref;
  uint8_t key;
};

int
sp::GenerateNativeThunk(MacroAssemblerX86& masm,
                        PluginRuntime* rt,
                        NativeCallContext caller,
                        NativeInfo* native,
                        uint32_t native_index,
                        uint32_t nparams,
                        Label* error)
{
  // Calculate stack space.
  const NativeSpec* spec = native->spec;

  TypeSpecIter rval(spec);
  assert(!rval.done());

  TypeSpecIter first_arg(rval.nextCopy());

  int32_t arg_stack = 0;
  uint32_t arg_count = 0;
  for (TypeSpecIter iter(first_arg); !iter.done(); iter.next()) {
    switch (iter.key) {
      case uint8_t(TypeSpec::int32):
      case uint8_t(TypeSpec::float32):
        arg_stack += 4;
        break;
      default:
        assert(false);
        return SP_ERROR_INVALID_NATIVE;
    }
    arg_count++;
  }

  // Argument counts must match.
  if (arg_count != nparams)
    return SP_ERROR_INVALID_NATIVE;

  if (spec->flags & NativeSpecFlags::HasPluginContext)
    arg_stack += 4;

  ExternalAddress hpAddr(rt->GetBaseContext()->addressOfHp());
  ExternalAddress spAddr(rt->GetBaseContext()->addressOfSp());

  // Start generating code.
  __ enterExitFrame(FrameType::NewNative);

  // Save ALT, HP, and store native_index for debugging.
  __ push(edx);
  __ push(Operand(hpAddr));
  __ push(native_index);

  // Compute misalignment - stack must be 16-byte aligned.
  int32_t total_stack = arg_stack + (3 * sizeof(intptr_t));
  uint32_t misalignment = Align(total_stack, kStackAlignment) - total_stack;
  if (misalignment)
    __ subl(esp, misalignment);

  int32_t arg_index = 0;
  for (TypeSpecIter iter(first_arg); !iter.done(); iter.next()) {
    switch (iter.key) {
      case uint8_t(TypeSpec::int32):
      case uint8_t(TypeSpec::float32):
        __ push(Operand(stk, arg_index * sizeof(cell_t)));
        break;
      default:
        assert(false);
        return SP_ERROR_INVALID_NATIVE;
    }
    arg_index++;
  }

  if (spec->flags & NativeSpecFlags::HasPluginContext)
    __ push(intptr_t(rt->GetBaseContext()));

  // Sync SP.
  __ subl(stk, dat);
  __ movl(Operand(spAddr), stk);

  // Invoke the native.
  __ call(ExternalAddress(spec->method));

  // Restore HP.
  int32_t save_base = arg_stack + misalignment;
  __ movl(edx, Operand(esp, save_base + 1 * sizeof(intptr_t)));
  __ movl(Operand(hpAddr), edx);

  // Restore ALT.
  __ movl(edx, Operand(esp, save_base + 2 * sizeof(intptr_t)));

  // Restore SP.
  __ addl(stk, dat);

  // Drop stack.
  __ addl(esp, arg_stack + misalignment + 3 * sizeof(intptr_t));

  // If the return value is a float, get the result into eax.
  assert(pri == eax);
  if (rval.key == uint8_t(TypeSpec::float32)) {
    __ subl(esp, sizeof(float));
    __ fstp32(Operand(esp, 0));
    __ pop(pri);
  }

  // Check for errors. Note we jump directly to the return stub since the
  // error has already been reported.
  __ movl(ecx, intptr_t(Environment::get()));
  __ cmpl(Operand(ecx, Environment::offsetOfExceptionCode()), 0);
  __ j(not_zero, error);
  return 0;
}