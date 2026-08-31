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
#include "code-stubs.h"

#include "debug-metadata.h"
#include "environment.h"
#include "linking.h"
#include "v2/x64/constants-x64.h"
#include "v2/x64/jit_x64.h"
#include "v2/runtime-helpers.h"
#include "x64/macro-assembler-x64.h"

namespace sp {

using namespace sp::v2;

#define __ masm.

bool CodeStubs::CompileInvokeStubV2() {
    MacroAssembler masm;
    __ enterFrame(JitFrameType::Entry, 0);

    // 8 bytes for re-alignment, and another 48 for temporaries. This is enough
    // for 7 locals. Since enterFrame pushes one value after setting rbp, our
    // first local starts at -16, not -8.
    __ subq(rsp, 8 + 48);

    // Four locals are used to preserve callee-saved registers.
    __ movq(Operand(rbp, -16), rbx);
    __ movq(Operand(rbp, -24), r12);
    __ movq(Operand(rbp, -32), r13);
    __ movq(Operand(rbp, -40), r15);
#ifdef _WIN32
    __ movq(Operand(rbp, -48), rsi);
    __ movq(Operand(rbp, -56), rdi);
#endif

    // Save the return address.
    __ movq(Operand(rbp, -48), ArgReg2);

    Environment* env = Environment::get();

    // Set up registers.
    __ movq(context_reg, ArgReg0);
    __ movq(env_reg, intptr_t(env));
    __ movq(dat_reg, env->virt_mem().map_base());
    __ movl(stk, Operand(env_reg, Environment::offsetOfSp()));
    __ lea(frm, Operand(stk, dat_reg, NoScale));

    // Call into compiled code.
    __ call(ArgReg1);

    // Store the rval.
    __ movq(rdx, Operand(rbp, -48));
    __ movl(Operand(rdx, 0), rax);

    Label ret;
    __ bind(&ret);

    // Restore the stack.
#ifdef _WIN32
    __ movq(rdi, Operand(rbp, -56));
    __ movq(rsi, Operand(rbp, -48));
#endif
    __ movq(r15, Operand(rbp, -40));
    __ movq(r13, Operand(rbp, -32));
    __ movq(r12, Operand(rbp, -24));
    __ movq(rbx, Operand(rbp, -16));

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
        __ setupExitFrame(ExitFrameType::Helper, 0);
        __ movl(ArgReg0, rax);
        __ callWithABI(ExternalAddress((void*)CompilerBase::InvokeReportError));
        __ leaveExitFrame();
        __ jmp(&return_to_invoke);
    }

    __ bind(&throw_timeout);
    __ setupExitFrame(ExitFrameType::Helper, 0);
    __ callWithABI(ExternalAddress((void*)CompilerBase::InvokeReportTimeout));
    __ leaveExitFrame();
    __ jmp(&return_reported_error);

    __ bind(&bounds_error);
    __ movl(ArgReg1, Operand(rsp, 12)); // bounds
    __ movl(ArgReg0, Operand(rsp, 8)); // index
    __ setupExitFrame(ExitFrameType::Helper, 0);
    __ callWithABI(ExternalAddress((void*)ReportOutOfBoundsError));
    __ leaveExitFrame();
    __ jmp(&return_reported_error);

    Label deferred_error;
    __ bind(&deferred_error);
    __ setupExitFrame(ExitFrameType::Helper, 0);
    __ callWithABI(ExternalAddress((void*)CompilerBase::DispatchDeferredReport));
    __ leaveExitFrame();
    __ jmp(&return_reported_error);

    __ bind(&return_reported_error);
    __ jmp(&return_to_invoke);

    __ bind(&return_to_invoke);
    {
        __ setupExitFrame(ExitFrameType::Helper, 0);
        __ callWithABI(ExternalAddress((void*)CompilerBase::FindEntryFp));
        __ leaveExitFrame();
        __ movq(rbp, rax);
        __ jmp(&error);
    }

    for (int i = 1; i < SP_MAX_ERROR_CODES; i++) {
        __ bind(&throw_error_code[i]);
        __ movl(rax, i);
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

    // Push our exit frame. This re-aligns the stack.
    __ setupExitFrame(ExitFrameType::Helper, 0);

    // ArgReg0 was set by our caller.
    __ callWithABI(ExternalAddress((void*)HeapItem::Destroy));

    __ leaveExitFrame();
    __ ret();

    dealloc_stub_ = LinkCode(env_, masm, "<dealloc stub>", {});
    return !!dealloc_stub_.entry;
}

} // namespace sp
