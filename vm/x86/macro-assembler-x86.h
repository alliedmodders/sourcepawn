// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2004-2026 AlliedModders LLC
//
#ifndef _include_sourcepawn_macroassembler_x86h__
#define _include_sourcepawn_macroassembler_x86h__

#include <amtl/am-vector.h>
#include <assembler.h>
#include <string.h>
#include "assembler-x86.h"
#include "environment.h"
#include "stack-frames.h"

namespace sp {

class MacroAssembler : public Assembler
{
  public:
    void enterFrame(JitFrameType type, uintptr_t function_id) {
        push(ebp);
        movl(ebp, esp);
        push(uint32_t(type));
        push(function_id);
    }
    void leaveFrame() {
        leave();
    }
    void enterExitFrame(ExitFrameType type, uintptr_t payload) {
        enterFrame(JitFrameType::Exit, EncodeExitFrameId(type, payload));
        movl(Operand(ExternalAddress(Environment::get()->addressOfExit())), ebp);
    }
    void leaveExitFrame() {
        leaveFrame();
    }

    void pushInlineExitFrame(ExitFrameType type, uintptr_t payload, CodeLabel* return_address) {
        push(return_address);
        push(ebp);
        movl(Operand(ExternalAddress(Environment::get()->addressOfExit())), esp);
        push(uint32_t(JitFrameType::Exit));
        push(EncodeExitFrameId(type, payload));
    }

    void popInlineExitFrame(uint32_t extra_argc) {
        addl(esp, (4 + extra_argc) * sizeof(uintptr_t));
    }

    void alignStack() {
        andl(esp, 0xfffffff0);
    }
    void assertStackAligned() {
#if defined(DEBUG)
        Label ok;
        testl(esp, 0xf);
        j(equal, &ok);
        breakpoint();
        bind(&ok);
#endif
    }

    template <typename T>
    void callWithABI(const T& target) {
        assertStackAligned();
        call(target);
    }
};

} // namespace sp

#endif // _include_sourcepawn_macroassembler_x86h__
