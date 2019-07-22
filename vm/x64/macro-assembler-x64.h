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
#ifndef _include_sourcepawn_macro_assembler_x64_h__
#define _include_sourcepawn_macro_assembler_x64_h__

#include <assembler.h>
#include "assembler-x64.h"
#include "stack-frames.h"
#include "constants-x64.h"

namespace sp {

// Extra words are type and function id.
static const intptr_t kExtraWordsInSpFrame = 2;

class ReserveScratch;

class MacroAssembler : public Assembler
{
  friend class ReserveScratch;

 public:
  MacroAssembler();

  void enterFrame(JitFrameType type, uint32_t function_id);
  void leaveFrame();

  void enterExitFrame(ExitFrameType type, uintptr_t payload);
  void leaveExitFrame();

  void assertStackAligned();

  void alignStack();

  using Assembler::movq;
  using Assembler::movl;
  void movq(const AddressOperand& dest, Register src);
  void movq(Register src, const AddressOperand& dest);
  void movl(const AddressOperand& dest, Register src);
  void movl(Register src, const AddressOperand& dest);

  using Assembler::cmpl;
  void cmpl(const AddressOperand& dest, int32_t imm);

  using Assembler::call;
  void call(const AddressValue& address);

  template <typename T>
  void callWithABI(const T& address) {
    assertStackAligned();
    call(address);
  }

  using Assembler::jmp;
  void jmp(const AddressValue& address);

 private:
  ReserveScratch* scratch_reserved_;
};

class ReserveScratch
{
 public:
  ReserveScratch(MacroAssembler* masm)
   : masm_(*masm)
  {
    assert(!masm_.scratch_reserved_);
    masm_.scratch_reserved_ = this;
  }
  ~ReserveScratch() {
    masm_.scratch_reserved_ = nullptr;
  }

  Register reg() const {
    return reserved_scratch;
  }

 private:
  MacroAssembler& masm_;
};

} // namespace sp

#endif // _include_sourcepawn_macro_assembler_x64_h__
