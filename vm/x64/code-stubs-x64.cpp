// vim: set sts=4 ts=8 sw=4 tw=99 et:
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
#include "constants-x64.h"
#include "debug-metadata.h"
#include "linking.h"
#include "macro-assembler-x64.h"
#include "plugin-runtime.h"

#define __ masm.

namespace sp {

// Windows ABI: RCX, RDX, R8, R9
// Linux ABI: RDI, RSI, RDX, RCX, R8, R9
//
// Trash registers common to both ABIs:
//     RAX, RCX, RDX, R8, R9, R10, R11
// Callee-saved common to both ABIs:
//     RBX, RBP, R12, R13, R14, R15

bool
CodeStubs::CompileInvokeStub()
{
    MacroAssembler masm;

    // Add 1 for the return address that was pushed.
    size_t frame_items = __ enterFrame(JitFrameType::Entry, 0) + 1;

    // Push all the callee-saved regs we clobber.
    __ push(context_reg);
    __ push(env_reg);
    __ push(stk);
    __ push(dat);
    __ push(frm);
    frame_items += 5;
#if defined(KE_WINDOWS)
    __ push(rdi);
    __ push(rsi);
    frame_items += 2;
#endif

    size_t frame_items_to_restore = frame_items;

    // arg0 = cx
    // arg1 = code
    // arg2 = rval
    __ movq(env_reg, intptr_t(Environment::get()));
    __ movq(context_reg, ArgReg0);
    __ push(ArgReg2);
    frame_items++;

    // Set up runtime registers.
    __ movq(dat, Operand(ArgReg0, static_cast<int32_t>(PluginContext::offsetOfMemory())));
    __ movl(stk, Operand(ArgReg0, static_cast<int32_t>(PluginContext::offsetOfSp())));
    __ addq(stk, dat);

    // We pushed 6 words.
    size_t alignment = PreCallStackAlignment(frame_items);
    if (alignment)
        __ subq(rsp, alignment);

    // Call into plugin.
    __ call(ArgReg1);

    // Store the rval.
    __ movq(ArgReg2, Operand(rsp, alignment));
    __ movl(Operand(ArgReg2, 0), pri);

    // Store latest stk. If we have an error code, we'll jump directly to here,
    // so rax will already be set.
    Label ret;
    __ bind(&ret);
    __ subq(stk, dat);
    __ movq(Operand(context_reg, static_cast<int32_t>(PluginContext::offsetOfSp())), stk);

    // Stack layout:
    //
    //      return_address
    //      rbp
    //      r10
    //      context_reg
    //      env_reg
    //      stk
    //      dat
    //      frm
    // #if defined(KE_WINDOWS)
    //      rdi
    //      rsi
    // #endif
    //      <-- restore RSP to here -->
    //      ArgReg2
    //      alignment
    //
    // The delta between rbp to frm (or rsi on Windows) is frame_items_to_restore
    // minus the entries for return_address and rbp.
    int32_t offset_to_rsp = (frame_items_to_restore - 2) * sizeof(intptr_t);
    __ lea(rsp, Operand(rbp, -offset_to_rsp));

#if defined(KE_WINDOWS)
    __ pop(rsi);
    __ pop(rdi);
#endif
    __ pop(frm);
    __ pop(dat);
    __ pop(stk);
    __ pop(env_reg);
    __ pop(context_reg);

    // Restore registers and leave.
    __ leaveFrame();
    __ ret();

    // The universal emergency return will jump to here.
    Label error;
    __ bind(&error);
    __ jmp(&ret);

    invoke_stub_ = LinkCode(env_, masm, "<jit invoke stub>", {});
    if (!invoke_stub_.entry)
        return false;

    return_stub_ = reinterpret_cast<uint8_t*>(invoke_stub_.entry) + error.offset();
    return true;
}

} // namespace sp
