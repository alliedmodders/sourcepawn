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
#include "macro-assembler-x64.h"
#include "environment.h"

namespace sp {

MacroAssembler::MacroAssembler()
 : scratch_reserved_(nullptr)
{
}

void
MacroAssembler::enterFrame(JitFrameType type, uint32_t function_id)
{
  push(rbp);
  movq(rbp, rsp);
  push(uint32_t(type));
  push(function_id);
}

void
MacroAssembler::leaveFrame()
{
  leave();
}

void
MacroAssembler::enterExitFrame(ExitFrameType type, uintptr_t payload)
{
  enterFrame(JitFrameType::Exit, EncodeExitFrameId(type, payload));
  movq(AddressOperand(Environment::get()->addressOfExit()), rbp);
}

void
MacroAssembler::leaveExitFrame()
{
  leaveFrame();
}

void
MacroAssembler::alignStack()
{
  andq(rsp, 0xfffffff0);
}

void
MacroAssembler::assertStackAligned()
{
#if defined(DEBUG)
  Label ok;
  testq(rsp, 0xf);
  j(equal, &ok);
  breakpoint();
  bind(&ok);
#endif
}

void
MacroAssembler::movq(const AddressOperand& dest, Register src)
{
  if (dest.has32BitEncoding() || src == rax) {
    Assembler::movq(dest, src);
  } else {
    ReserveScratch scratch(this);
    movq(scratch.reg(), dest.asValue());
    movq(Operand(scratch.reg(), 0), src);
  }
}

void
MacroAssembler::movq(Register dest, const AddressOperand& src)
{
  if (src.has32BitEncoding() || dest == rax) {
    Assembler::movq(dest, src);
  } else {
    ReserveScratch scratch(this);
    movq(scratch.reg(), src.asValue());
    movq(dest, Operand(scratch.reg(), 0));
  }
}

void
MacroAssembler::movl(const AddressOperand& dest, Register src)
{
  if (dest.has32BitEncoding()) {
    Assembler::movl(Operand(dest.asValue()), src);
  } else {
    ReserveScratch scratch(this);
    movq(scratch.reg(), dest.asValue());
    movl(Operand(scratch.reg(), 0), src);
  }
}

void
MacroAssembler::movl(Register dest, const AddressOperand& src)
{
  if (src.has32BitEncoding()) {
    Assembler::movl(dest, Operand(src.asValue()));
  } else {
    ReserveScratch scratch(this);
    movq(scratch.reg(), src.asValue());
    movl(dest, Operand(scratch.reg(), 0));
  }
}

void
MacroAssembler::cmpl(const AddressOperand& dest, int32_t imm)
{
  if (dest.has32BitEncoding()) {
    cmpl(Operand(dest.asValue()), imm);
  } else {
    ReserveScratch scratch(this);
    movq(scratch.reg(), dest.asValue());
    cmpl(Operand(scratch.reg(), 0), 0);
  }
}

void
MacroAssembler::call(const AddressValue& address)
{
  ReserveScratch scratch(this);
  movq(scratch.reg(), address);
  call(scratch.reg());
}

void
MacroAssembler::jmp(const AddressValue& address)
{
  ReserveScratch scratch(this);
  movq(scratch.reg(), address);
  jmp(scratch.reg());
}

} // namespace sp
