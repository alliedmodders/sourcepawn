// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
//
#include "macro-assembler-x64.h"

#include "environment.h"

namespace sp {

MacroAssembler::MacroAssembler()
{}

size_t MacroAssembler::enterFrame(JitFrameType type, uint32_t function_id) {
    push(rbp);
    movq(rbp, rsp);
    // Use r10 since it doesn't conflict with any argument register on x64/x86
    movq(r10, (uintptr_t(function_id) << 32) | uintptr_t(type));
    push(r10);
    return 2;
}

void
MacroAssembler::leaveFrame() {
    leave();
}

size_t MacroAssembler::enterExitFrame(ExitFrameType type, uintptr_t payload) {
    size_t items = enterFrame(JitFrameType::Exit, EncodeExitFrameId(type, payload));
    movq(Operand(env_reg, Environment::offsetOfExit()), rbp);
    return items;
}

void MacroAssembler::setupExitFrame(ExitFrameType type, uintptr_t payload) {
    enterExitFrame(type, payload);
#ifdef _WIN64
    // Need to re-align the stack, and add an extra 32 bytes since the ABI
    // requires shadow spill space.
    subq(rsp, 40);
#else
    subq(rsp, 8);
#endif
}

void
MacroAssembler::leaveExitFrame() {
    leaveFrame();
}

void
MacroAssembler::assertStackAligned() {
#if defined(DEBUG)
    Label ok;
    testq(rsp, 0xf);
    j(equal, &ok);
    breakpoint();
    bind(&ok);
#endif
}

} // namespace sp
