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
