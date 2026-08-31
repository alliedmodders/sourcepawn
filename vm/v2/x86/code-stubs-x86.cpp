// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
//
#include "code-stubs.h"

#include <sp_vm_api.h>
#include "debug-metadata.h"
#include "environment.h"
#include "linking.h"
#include "v2/runtime-helpers.h"
#include "v2/x86/jit_x86.h"

namespace sp {

using namespace sp::v2;

#define __ masm.

bool CodeStubs::CompileInvokeStubV2() {
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

    // Set up run-time registers.
    __ movl(edx, intptr_t(Environment::get()));
    __ movl(stk, Operand(edx, Environment::offsetOfSp()));
    __ movl(frm, stk);

    // Align the stack.
    __ subl(esp, 16);
    __ andl(esp, 0xfffffff0);

    // Call into plugin.
    __ call(ecx);

    // Store the rval.
    __ movl(ecx, Operand(ebp, kRvalOffset));
    __ movl(Operand(ecx, 0), eax);

    Label ret;
    __ bind(&ret);

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

    Label report_error;
    Label throw_timeout;
    Label return_reported_error;
    Label return_to_invoke;
    Label bounds_error;
    Label throw_error_code[SP_MAX_ERROR_CODES];

    __ bind(&report_error);
    {
        __ enterExitFrame(ExitFrameType::Helper, 0);
        __ subl(esp, 12);
        __ push(eax);
        __ callWithABI(ExternalAddress((void*)CompilerBase::InvokeReportError));
        __ leaveExitFrame();
        __ jmp(&return_to_invoke);
    }

    __ bind(&throw_timeout);
    __ enterExitFrame(ExitFrameType::Helper, 0);
    __ callWithABI(ExternalAddress((void*)CompilerBase::InvokeReportTimeout));
    __ leaveExitFrame();
    __ jmp(&return_reported_error);

    __ bind(&bounds_error);
    __ movl(eax, Operand(esp, 8)); // bounds
    __ movl(ecx, Operand(esp, 4)); // index
    __ enterExitFrame(ExitFrameType::Helper, 0);
    __ subl(esp, 16);
    __ movl(Operand(esp, 4), eax);
    __ movl(Operand(esp, 0), ecx);
    __ callWithABI(ExternalAddress((void*)ReportOutOfBoundsError));
    __ leaveExitFrame();
    __ jmp(&return_reported_error);

    Label deferred_error;
    __ bind(&deferred_error);
    __ enterExitFrame(ExitFrameType::Helper, 0);
    __ callWithABI(ExternalAddress((void*)CompilerBase::DispatchDeferredReport));
    __ leaveExitFrame();
    __ jmp(&return_reported_error);

    __ bind(&return_reported_error);
    __ jmp(&return_to_invoke);

    __ bind(&return_to_invoke);
    {
        __ enterExitFrame(ExitFrameType::Helper, 0);
        __ subl(esp, 12);
        __ push(ebx);
        __ callWithABI(ExternalAddress((void*)CompilerBase::UnwindStack));
        __ leaveExitFrame();
        __ movl(ebp, eax);
        __ jmp(&error);
    }

    for (int i = 1; i < SP_MAX_ERROR_CODES; i++) {
        __ bind(&throw_error_code[i]);
        __ movl(eax, i);
        __ jmp(&report_error);
    }

    invoke_stub_v2_ = LinkCode(env_, masm, "<jit invoke stub>", {});
    if (!invoke_stub_v2_.entry)
        return false;

    uint8_t* entry = reinterpret_cast<uint8_t*>(invoke_stub_v2_.entry);
    return_stubs_v2_.report_error = entry + report_error.offset();
    return_stubs_v2_.throw_timeout = entry + throw_timeout.offset();
    return_stubs_v2_.return_reported_error = entry + return_reported_error.offset();
    return_stubs_v2_.bounds_error = entry + bounds_error.offset();
    return_stubs_v2_.deferred_error = entry + deferred_error.offset();
    for (int i = 1; i < SP_MAX_ERROR_CODES; i++)
        return_stubs_v2_.throw_error_code[i] = entry + throw_error_code[i].offset();

    return true;
}

bool CodeStubs::CompileDeallocStub() {
    MacroAssembler masm;

    // Grab the dead object into ecx.
    __ movl(ecx, Operand(esp, 4));

    // Push our exit frame. This re-aligns the stack.
    __ enterExitFrame(ExitFrameType::Helper, 0);

    __ subl(esp, 16);
    __ movl(Operand(esp, 0), ecx);
    __ callWithABI(ExternalAddress((void*)HeapItem::Destroy));
    __ addl(esp, 16);

    __ leaveExitFrame();
    __ ret();

    dealloc_stub_ = LinkCode(env_, masm, "<dealloc stub>", {});
    return !!dealloc_stub_.entry;
}

} // namespace sp
