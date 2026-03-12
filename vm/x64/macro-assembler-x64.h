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
#ifndef _include_sourcepawn_macro_assembler_x64_h__
#define _include_sourcepawn_macro_assembler_x64_h__

#include <amtl/am-bits.h>
#include "assembler-x64.h"
#include "assembler.h"
#include "constants-x64.h"
#include "environment.h"
#include "stack-frames.h"

namespace sp {

// Extra words are type and function id.
static const intptr_t kExtraWordsInSpFrame = 2;

// Shadow stack size.
#if defined(KE_WINDOWS)
static const intptr_t kShadowStackSize = 32;
#else
static const intptr_t kShadowStackSize = 0;
#endif


// Compute the amount of stack to reserve before a call, to (1) reserve shadow
// space (on Windows) and (2) to fix alignment. slots_used is the number of
// 8-byte stack increments that have been added on top of an aligned stack.
//
// For example, in the prologue of a function, this should be 1 as the CALL
// instruction is aligned, and the return address is pushed immediately after,
// causing misalignment for the next CALL.
static inline size_t PreCallStackAlignment(size_t slots_used) {
    return kShadowStackSize + ((slots_used % 2) ? sizeof(intptr_t) : 0);
}

// Same as above, but allows reserving additional space. The tail kShadowStackSize
// amount must not be used.
static inline size_t AllocatePreCallStack(size_t unaligned_bytes_needed, size_t slots_pushed) {
    size_t extra_needed = ke::Align(unaligned_bytes_needed, sizeof(intptr_t));
    size_t already_pushed = slots_pushed * sizeof(intptr_t);
    size_t aligned = ke::Align(extra_needed + already_pushed, 16) - already_pushed;
    return aligned + kShadowStackSize;
}

class MacroAssembler : public Assembler
{
  public:
    MacroAssembler();

    // Returns the number of items added to the stack. This does not include
    // the return address that was implicitly pushed.
    size_t enterFrame(JitFrameType type, uint32_t function_id);
    void leaveFrame();

    // Returns the number of items added to the stack.
    size_t enterExitFrame(ExitFrameType type, uintptr_t payload);
    void leaveExitFrame();

    void assertStackAligned();
    void alignStack();

    // Clobber rcx.
    // Returns the number of items added to the stack.
    size_t pushInlineExitFrame(ExitFrameType type, uintptr_t payload, RipCodeLabel* return_address) {
        lea(rcx, return_address);
        push(rcx);
        push(rbp);
        movq(Operand(env_reg, Environment::offsetOfExit()), rsp);
        movq(rcx, (EncodeExitFrameId(type, payload) << 32) | uintptr_t(JitFrameType::Exit));
        push(rcx);
        return 3;
    }

    void popInlineExitFrame(size_t extra_stack = 0) {
        addq(rsp, (3 * sizeof(uintptr_t)) + extra_stack);
    }

    using Assembler::movl;
    using Assembler::movq;
    void movq(const AddressOperand& dest, Register src);
    void movq(Register src, const AddressOperand& dest);
    void movl(const AddressOperand& dest, Register src);
    void movl(Register src, const AddressOperand& dest);

    using Assembler::cmpl;
    void cmpl(const AddressOperand& dest, int32_t imm);

    template <typename T>
    void callWithABI(const T& address) {
        assertStackAligned();
        call(address);
    }

    // Do not use this for calls that expect arguments on the stack, since this
    // merges the shadow stack (on Windows) with the stack alignment. If there
    // are arguments passed, Windows expects there to be no buffer in between
    // the shadow space and the first stack argument.
    template <typename T>
    size_t callWithABI(size_t words_pushed, const T& address) {
        size_t alignment = PreCallStackAlignment(words_pushed);
        if (alignment)
            subq(rsp, alignment);
        assertStackAligned();
        call(address);
        return alignment;
    }
};

} // namespace sp

#endif // _include_sourcepawn_macro_assembler_x64_h__
