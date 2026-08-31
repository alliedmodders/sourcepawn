// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
//
#include <sp_vm_api.h>
#include "code-stubs.h"
#include "debug-metadata.h"
#include "environment.h"
#include "legacy/x86/jit_x86.h"
#include "linking.h"

namespace sp {

using namespace sp::v1;

#define __ masm.

bool CodeStubs::CompileInvokeStubV1() {
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

    invoke_stub_v1_ = LinkCode(env_, masm, "<jit invoke stub>", {});
    if (!invoke_stub_v1_.entry)
        return false;

    return_stub_ = reinterpret_cast<uint8_t*>(invoke_stub_v1_.entry) + error.offset();
    return true;
}

} // namespace sp
