/**
 * vim: set ts=2 sw=2 tw=99 et:
 * =============================================================================
 * SourceMod
 * Copyright (C) 2004-2008 AlliedModders LLC.  All rights reserved.
 * =============================================================================
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License, version 3.0, as published by the
 * Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * As a special exception, AlliedModders LLC gives you permission to link the
 * code of this program (as well as its derivative works) to "Half-Life 2," the
 * "Source Engine," the "SourcePawn JIT," and any Game MODs that run on software
 * by the Valve Corporation.  You must obey the GNU General Public License in
 * all respects for all other code used.  Additionally, AlliedModders LLC grants
 * this exception to all derivative works.  AlliedModders LLC defines further
 * exceptions, found in LICENSE.txt (as of this writing, version JULY-31-2007),
 * or <http://www.sourcemod.net/license.php>.
 *
 * Version: $Id$
 */

#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "jit_x86.h"
#include "plugin-runtime.h"
#include "plugin-context.h"
#include "watchdog_timer.h"
#include "environment.h"
#include "code-stubs.h"
#include "linking.h"
#include "frames-x86.h"
#include "outofline-asm.h"
#include "method-info.h"
#include "runtime-helpers.h"
#include "debugging.h"

#define __ masm.

namespace sp {

static inline ConditionCode
OpToCondition(CompareOp op)
{
  switch (op) {
  case CompareOp::Eq:
    return equal;
  case CompareOp::Neq:
    return not_equal;
  case CompareOp::Sless:
    return less;
  case CompareOp::Sleq:
    return less_equal;
  case CompareOp::Sgrtr:
    return greater;
  case CompareOp::Sgeq:
    return greater_equal;
  default:
    assert(false);
    return negative;
  }
}

Compiler::Compiler(PluginRuntime* rt, MethodInfo* method)
 : CompilerBase(rt, method)
{
}

Compiler::~Compiler()
{
}

// No exit frame - error code is returned directly.
static int
InvokePushTracker(PluginContext* cx, uint32_t amount)
{
  return cx->pushTracker(amount);
}

// No exit frame - error code is returned directly.
static int
InvokePopTrackerAndSetHeap(PluginContext* cx)
{
  return cx->popTrackerAndSetHeap();
}

// No exit frame - error code is returned directly.
static int
InvokeGenerateFullArray(PluginContext* cx, uint32_t argc, cell_t* argv, int autozero)
{
  return cx->generateFullArray(argc, argv, autozero);
}

static int
InvokeInitArray(PluginContext* cx,
                cell_t base_addr,
                cell_t dat_addr,
                cell_t iv_size,
                cell_t data_copy_size,
                cell_t data_fill_size,
                cell_t fill_value)
{
  return cx->initArray(base_addr, dat_addr, iv_size, data_copy_size, data_fill_size,
                       fill_value) ? 1 : 0;
}

bool
Compiler::visitMOVE(PawnReg reg)
{
  if (reg == PawnReg::Pri)
    __ movl(pri, alt);
  else
    __ movl(alt, pri);
  return true;
}

bool
Compiler::visitXCHG()
{
  __ xchgl(pri, alt);
  return true;
}

bool
Compiler::visitZERO(cell_t offset)
{
  __ movl(Operand(dat, offset), 0);
  return true;
}

bool
Compiler::visitZERO_S(cell_t offset)
{
  __ movl(Operand(frm, offset), 0);
  return true;
}

bool
Compiler::visitPUSH(PawnReg src)
{
  Register reg = (src == PawnReg::Pri) ? pri : alt;
  __ movl(Operand(stk, -4), reg);
  __ subl(stk, 4);
  return true;
}

bool
Compiler::visitPUSH_C(const cell_t* vals, size_t nvals)
{
  for (size_t i = 1; i <= nvals; i++)
    __ movl(Operand(stk, -(4 * int(i))), vals[i - 1]);
  __ subl(stk, 4 * nvals);
  return true;
}

bool
Compiler::visitPUSH_ADR(const cell_t* offsets, size_t nvals)
{
  // We temporarily relocate FRM to be a local address instead of an
  // absolute address.
  __ subl(frm, dat);
  for (size_t i = 1; i <= nvals; i++) {
    __ lea(tmp, Operand(frm, offsets[i - 1]));
    __ movl(Operand(stk, -(4 * int(i))), tmp);
  }
  __ subl(stk, 4 * nvals);
  __ addl(frm, dat);
  return true;
}

bool
Compiler::visitPUSH_S(const cell_t* offsets, size_t nvals)
{
  for (size_t i = 1; i <= nvals; i++) {
    __ movl(tmp, Operand(frm, offsets[i - 1]));
    __ movl(Operand(stk, -(4 * int(i))), tmp);
  }
  __ subl(stk, 4 * nvals);
  return true;
}

bool
Compiler::visitPUSH(const cell_t* offsets, size_t nvals)
{
  for (size_t i = 1; i <= nvals; i++) {
    __ movl(tmp, Operand(dat, offsets[i - 1]));
    __ movl(Operand(stk, -(4 * int(i))), tmp);
  }
  __ subl(stk, 4 * nvals);
  return true;
}

bool
Compiler::visitZERO(PawnReg dest)
{
  Register reg = (dest == PawnReg::Pri) ? pri : alt;
  __ xorl(reg, reg);
  return true;
}

bool
Compiler::visitADD()
{
  __ addl(pri, alt);
  return true;
}

bool
Compiler::visitSUB()
{
  __ subl(pri, alt);
  return true;
}

bool
Compiler::visitSUB_ALT()
{
  __ movl(tmp, alt);
  __ subl(tmp, pri);
  __ movl(pri, tmp);
  return true;
}

void
Compiler::emitPrologue()
{
  __ enterFrame(JitFrameType::Scripted, pcode_start_);

  // Push the old frame onto the stack.
  __ subl(stk, 8);
  __ movl(tmp, Operand(frmAddr()));
  __ movl(Operand(stk, 4), tmp);
  __ movl(tmp, Operand(hpAddr()));
  __ movl(Operand(stk, 0), tmp);

  // Get and store the new frame.
  __ movl(tmp, stk);
  __ movl(frm, stk);
  __ subl(tmp, dat);
  __ movl(Operand(frmAddr()), tmp);

  int32_t max_stack = method_info_->max_stack();
  assert(max_stack >= 0);

  if (max_stack) {
    __ movl(eax, Operand(hpAddr()));
    __ lea(eax, Operand(dat, eax, NoScale, STACK_MARGIN));
    __ lea(ecx, Operand(stk, -max_stack));
    __ cmpl(ecx, eax);
    jumpOnError(below, SP_ERROR_STACKLOW);
  }
}

bool
Compiler::visitSHL()
{
  __ movl(ecx, alt);
  __ shll_cl(pri);
  return true;
}

bool
Compiler::visitSHR()
{
  __ movl(ecx, alt);
  __ shrl_cl(pri);
  return true;
}

bool
Compiler::visitSSHR()
{
  __ movl(ecx, alt);
  __ sarl_cl(pri);
  return true;
}

bool
Compiler::visitSHL_C(PawnReg dest, cell_t amount)
{
  Register reg = (dest == PawnReg::Pri) ? pri : alt;
  __ shll(reg, amount);
  return true;
}

bool
Compiler::visitSMUL()
{
  __ imull(pri, alt);
  return true;
}

bool
Compiler::visitNOT()
{
  __ testl(eax, eax);
  __ movl(eax, 0);
  __ set(zero, r8_al);
  return true;
}

bool
Compiler::visitNEG()
{
  __ negl(eax);
  return true;
}

bool
Compiler::visitXOR()
{
  __ xorl(pri, alt);
  return true;
}

bool
Compiler::visitOR()
{
  __ orl(pri, alt);
  return true;
}

bool
Compiler::visitAND()
{
  __ andl(pri, alt);
  return true;
}

bool
Compiler::visitINVERT()
{
  __ notl(pri);
  return true;
}

bool
Compiler::visitADD_C(cell_t value)
{
  __ addl(pri, value);
  return true;
}

bool
Compiler::visitSMUL_C(cell_t value)
{
  __ imull(pri, pri, value);
  return true;
}

bool
Compiler::visitCompareOp(CompareOp op)
{
  ConditionCode cc = OpToCondition(op);
  __ cmpl(pri, alt);
  __ movl(pri, 0);
  __ set(cc, r8_al);
  return true;
}

bool
Compiler::visitEQ_C(PawnReg src, cell_t value)
{
  Register reg = (src == PawnReg::Pri) ? pri : alt;
  __ cmpl(reg, value);
  __ movl(pri, 0);
  __ set(equal, r8_al);
  return true;
}

bool
Compiler::visitINC(PawnReg dest)
{
  Register reg = (dest == PawnReg::Pri) ? pri : alt;
  __ addl(reg, 1);
  return true;
}

bool
Compiler::visitINC(cell_t offset)
{
  __ addl(Operand(dat, offset), 1);
  return true;
}

bool
Compiler::visitINC_S(cell_t offset)
{
  __ addl(Operand(frm, offset), 1);
  return true;
}

bool
Compiler::visitINC_I()
{
  __ addl(Operand(dat, pri, NoScale), 1);
  return true;
}

bool
Compiler::visitDEC(PawnReg dest)
{
  Register reg = (dest == PawnReg::Pri) ? pri : alt;
  __ subl(reg, 1);
  return true;
}

bool
Compiler::visitDEC(cell_t offset)
{
  __ subl(Operand(dat, offset), 1);
  return true;
}

bool
Compiler::visitDEC_S(cell_t offset)
{
  __ subl(Operand(frm, offset), 1);
  return true;
}

bool
Compiler::visitDEC_I()
{
  __ subl(Operand(dat, pri, NoScale), 1);
  return true;
}

bool
Compiler::visitLOAD(PawnReg dest, cell_t srcaddr)
{
  Register reg = (dest == PawnReg::Pri) ? pri : alt;
  __ movl(reg, Operand(dat, srcaddr));
  return true;
}

bool
Compiler::visitLOAD_BOTH(cell_t offsetForPri, cell_t offsetForAlt)
{
  visitLOAD(PawnReg::Pri, offsetForPri);
  visitLOAD(PawnReg::Alt, offsetForAlt);
  return true;
}

bool
Compiler::visitLOAD_S(PawnReg dest, cell_t srcoffs)
{
  Register reg = (dest == PawnReg::Pri) ? pri : alt;
  __ movl(reg, Operand(frm, srcoffs));
  return true;
}

bool
Compiler::visitLOAD_S_BOTH(cell_t offsetForPri, cell_t offsetForAlt)
{
  visitLOAD_S(PawnReg::Pri, offsetForPri);
  visitLOAD_S(PawnReg::Alt, offsetForAlt);
  return true;
}

bool
Compiler::visitLREF_S(PawnReg dest, cell_t srcoffs)
{
  Register reg = (dest == PawnReg::Pri) ? pri : alt;
  __ movl(reg, Operand(frm, srcoffs));
  __ movl(reg, Operand(dat, reg, NoScale));
  return true;
}

bool
Compiler::visitCONST(PawnReg dest, cell_t val)
{
  Register reg = (dest == PawnReg::Pri) ? pri : alt;
  __ movl(reg, val);
  return true;
}

bool
Compiler::visitADDR(PawnReg dest, cell_t offset)
{
  Register reg = (dest == PawnReg::Pri) ? pri : alt;
  __ movl(reg, Operand(frmAddr()));
  __ addl(reg, offset);
  return true;
}

bool
Compiler::visitSTOR(cell_t offset, PawnReg src)
{
  Register reg = (src == PawnReg::Pri) ? pri : alt;
  __ movl(Operand(dat, offset), reg);
  return true;
}

bool
Compiler::visitSTOR_S(cell_t offset, PawnReg src)
{
  Register reg = (src == PawnReg::Pri) ? pri : alt;
  __ movl(Operand(frm, offset), reg);
  return true;
}

bool
Compiler::visitIDXADDR()
{
  __ lea(pri, Operand(alt, pri, ScaleFour));
  return true;
}

bool
Compiler::visitSREF_S(cell_t offset, PawnReg src)
{
  Register reg = (src == PawnReg::Pri) ? pri : alt;
  __ movl(tmp, Operand(frm, offset));
  __ movl(Operand(dat, tmp, NoScale), reg);
  return true;
}

bool
Compiler::visitPOP(PawnReg dest)
{
  Register reg = (dest == PawnReg::Pri) ? pri : alt;
  __ movl(reg, Operand(stk, 0));
  __ addl(stk, 4);
  return true;
}

bool
Compiler::visitSWAP(PawnReg dest)
{
  Register reg = (dest == PawnReg::Pri) ? pri : alt;
  __ movl(tmp, Operand(stk, 0));
  __ movl(Operand(stk, 0), reg);
  __ movl(reg, tmp);
  return true;
}

bool
Compiler::visitLIDX()
{
  __ lea(pri, Operand(alt, pri, ScaleFour));
  __ movl(pri, Operand(dat, pri, NoScale));
  return true;
}

bool
Compiler::visitCONST(cell_t offset, cell_t value)
{
  __ movl(Operand(dat, offset), value);
  return true;
}

bool
Compiler::visitCONST_S(cell_t offset, cell_t value)
{
  __ movl(Operand(frm, offset), value);
  return true;
}

bool
Compiler::visitLOAD_I()
{
  emitCheckAddress(pri);
  __ movl(pri, Operand(dat, pri, NoScale));
  return true;
}

bool
Compiler::visitSTOR_I()
{
  emitCheckAddress(alt);
  __ movl(Operand(dat, alt, NoScale), pri);
  return true;
}

bool
Compiler::visitSDIV(PawnReg dest)
{
  Register dividend = (dest == PawnReg::Pri) ? pri : alt;
  Register divisor = (dest == PawnReg::Pri) ? alt : pri;

  // Guard against divide-by-zero.
  __ testl(divisor, divisor);
  jumpOnError(zero, SP_ERROR_DIVIDE_BY_ZERO);

  // A more subtle case; -INT_MIN / -1 yields an overflow exception.
  Label ok;
  __ cmpl(divisor, -1);
  __ j(not_equal, &ok);
  __ cmpl(dividend, 0x80000000);
  jumpOnError(equal, SP_ERROR_INTEGER_OVERFLOW);
  __ bind(&ok);

  // Now we can actually perform the divide.
  __ movl(tmp, divisor);
  if (dest == PawnReg::Pri)
    __ movl(edx, dividend);
  else
    __ movl(eax, dividend);
  __ sarl(edx, 31);
  __ idivl(tmp);
  return true;
}

bool
Compiler::visitLODB_I(cell_t width)
{
  emitCheckAddress(pri);
  __ movl(pri, Operand(dat, pri, NoScale));
  if (width == 1)
    __ andl(pri, 0xff);
  else if (width == 2)
    __ andl(pri, 0xffff);
  return true;
}

bool
Compiler::visitSTRB_I(cell_t width)
{
  emitCheckAddress(alt);
  if (width == 1)
    __ movb(Operand(dat, alt, NoScale), pri);
  else if (width == 2)
    __ movw(Operand(dat, alt, NoScale), pri);
  else if (width == 4)
    __ movl(Operand(dat, alt, NoScale), pri);
  return true;
}

bool
Compiler::visitRETN()
{
  for (uint32_t i = 0; i < block_->heap_scope_depth(); i++)
    visitHEAP_RESTORE();

  // Restore the old stack and frame pointer.
  __ movl(stk, frm);
  __ movl(frm, Operand(stk, 4));              // get the old frm
  __ movl(tmp, Operand(stk, 0));              // get the old hp
  __ movl(Operand(hpAddr()), tmp);
  __ addl(stk, 8);                            // pop stack
  __ movl(Operand(frmAddr()), frm);           // store back old frm
  __ addl(frm, dat);                          // relocate

  // Remove parameters.
  __ movl(tmp, Operand(stk, 0));
  __ lea(stk, Operand(stk, tmp, ScaleFour, 4));

  __ leaveFrame();
  __ ret();
  return true;
}

bool
Compiler::visitMOVS(uint32_t amount)
{
  unsigned dwords = amount / 4;
  unsigned bytes = amount % 4;

  __ cld();
  __ push(esi);
  __ push(edi);
  // Note: set edi first, since we need esi.
  __ lea(edi, Operand(dat, alt, NoScale));
  __ lea(esi, Operand(dat, pri, NoScale));
  if (dwords) {
    __ movl(ecx, dwords);
    __ rep_movsd();
  }
  if (bytes) {
    __ movl(ecx, bytes);
    __ rep_movsb();
  }
  __ pop(edi);
  __ pop(esi);
  return true;
}
  

bool
Compiler::visitFILL(uint32_t amount)
{
  // eax/pri is used implicitly.
  unsigned dwords = amount / 4;
  __ push(edi);
  __ lea(edi, Operand(dat, alt, NoScale));
  __ movl(ecx, dwords);
  __ cld();
  __ rep_stosd();
  __ pop(edi);
  return true;
}

bool
Compiler::visitSTRADJUST_PRI()
{
  __ addl(pri, 4);
  __ sarl(pri, 2);
  return true;
}

bool
Compiler::visitFABS()
{
  __ movl(pri, Operand(stk, 0));
  __ andl(pri, 0x7fffffff);
  __ addl(stk, 4);
  return true;
}

bool
Compiler::visitFLOAT()
{
  if (MacroAssembler::Features().sse2) {
    __ cvtsi2ss(xmm0, Operand(edi, 0));
    __ movd(pri, xmm0);
  } else {
    __ fild32(Operand(edi, 0));
    __ subl(esp, 4);
    __ fstp32(Operand(esp, 0));
    __ pop(pri);
  }
  __ addl(stk, 4);
  return true;
}

bool
Compiler::visitFLOATADD()
{
  if (MacroAssembler::Features().sse2) {
    __ movss(xmm0, Operand(stk, 0));
    __ addss(xmm0, Operand(stk, 4));
    __ movd(pri, xmm0);
  } else {
    __ subl(esp, 4);
    __ fld32(Operand(stk, 0));
    __ fadd32(Operand(stk, 4));
    __ fstp32(Operand(esp, 0));
    __ pop(pri);
  }
  __ addl(stk, 8);
  return true;
}

bool
Compiler::visitFLOATSUB()
{
  if (MacroAssembler::Features().sse2) {
    __ movss(xmm0, Operand(stk, 0));
    __ subss(xmm0, Operand(stk, 4));
    __ movd(pri, xmm0);
  } else {
    __ subl(esp, 4);
    __ fld32(Operand(stk, 0));
    __ fsub32(Operand(stk, 4));
    __ fstp32(Operand(esp, 0));
    __ pop(pri);
  }
  __ addl(stk, 8);
  return true;
}

bool
Compiler::visitFLOATMUL()
{
  if (MacroAssembler::Features().sse2) {
    __ movss(xmm0, Operand(stk, 0));
    __ mulss(xmm0, Operand(stk, 4));
    __ movd(pri, xmm0);
  } else {
    __ subl(esp, 4);
    __ fld32(Operand(stk, 0));
    __ fmul32(Operand(stk, 4));
    __ fstp32(Operand(esp, 0));
    __ pop(pri);
  }
  __ addl(stk, 8);
  return true;
}

bool
Compiler::visitFLOATDIV()
{
  if (MacroAssembler::Features().sse2) {
    __ movss(xmm0, Operand(stk, 0));
    __ divss(xmm0, Operand(stk, 4));
    __ movd(pri, xmm0);
  } else {
    __ subl(esp, 4);
    __ fld32(Operand(stk, 0));
    __ fdiv32(Operand(stk, 4));
    __ fstp32(Operand(esp, 0));
    __ pop(pri);
  }
  __ addl(stk, 8);
  return true;
}

bool
Compiler::visitRND_TO_NEAREST()
{
  // Docs say that MXCSR must be preserved across function calls, so we
  // assume that we'll always get the defualt round-to-nearest.
  if (MacroAssembler::Features().sse) {
    __ cvtss2si(pri, Operand(stk, 0));
  } else {
    __ fld32(Operand(stk, 0));
    __ subl(esp, 4);
    __ fistp32(Operand(esp, 0));
    __ pop(pri);
  }
  __ addl(stk, 4);
  return true;
}

bool
Compiler::visitRND_TO_CEIL()
{
  // Adapted from http://wurstcaptures.untergrund.net/assembler_tricks.html#fastfloorf
  // (the above does not support the full integer range)
  static float kRoundToCeil = -0.5f;
  __ fld32(Operand(stk, 0));
  __ fadd32(st0, st0);
  __ fsubr32(Operand(ExternalAddress(&kRoundToCeil)));
  __ subl(esp, 8);
  __ fistp64(Operand(esp, 0));
  __ pop(eax); // low word
  __ pop(ecx); // high word
  // divide 64-bit integer by 2 (shift right by 1)
  __ shrd(eax, ecx, 1);
  __ sarl(ecx, 1);
  // negate 64-bit integer in eax:ecx
  __ negl(eax);
  __ adcl(ecx, 0);
  __ negl(ecx);
  // did this overflow? if so, return 0x80000000
  Label ok;
  __ testl(ecx, ecx);
  __ j(zero, &ok);
  __ cmpl(ecx, -1);
  __ j(equal, &ok);
  __ movl(pri, 0x80000000);
  __ bind(&ok);
  __ addl(stk, 4);
  return true;
}

bool
Compiler::visitRND_TO_ZERO() 
{
  if (MacroAssembler::Features().sse) {
    __ cvttss2si(pri, Operand(stk, 0));
  } else {
    __ fld32(Operand(stk, 0));
    __ subl(esp, 8);
    __ fstcw(Operand(esp, 4));
    __ movl(Operand(esp, 0), 0xfff);
    __ fldcw(Operand(esp, 0));
    __ fistp32(Operand(esp, 0));
    __ pop(pri);
    __ fldcw(Operand(esp, 0));
    __ addl(esp, 4);
  }
  __ addl(stk, 4);
  return true;
}

bool
Compiler::visitRND_TO_FLOOR()
{
  __ fld32(Operand(stk, 0));
  __ subl(esp, 8);
  __ fstcw(Operand(esp, 4));
  __ movl(Operand(esp, 0), 0x7ff);
  __ fldcw(Operand(esp, 0));
  __ fistp32(Operand(esp, 0));
  __ pop(eax);
  __ fldcw(Operand(esp, 0));
  __ addl(esp, 4);
  __ addl(stk, 4);
  return true;
}

bool
Compiler::visitFLOATCMP()
{
  // This is the old float cmp, which returns ordered results. In newly
  // compiled code it should not be used or generated.
  //
  // Note that the checks here are inverted: the test is |rhs OP lhs|.
  Label bl, ab, done;
  if (MacroAssembler::Features().sse) {
    __ movss(xmm0, Operand(stk, 4));
    __ ucomiss(Operand(stk, 0), xmm0);
  } else {
    __ fld32(Operand(stk, 0));
    __ fld32(Operand(stk, 4));
    __ fucomip(st1);
    __ fstp(st0);
  }
  __ j(above, &ab);
  __ j(below, &bl);
  __ xorl(pri, pri);
  __ jmp(&done);
  __ bind(&ab);
  __ movl(pri, -1);
  __ jmp(&done);
  __ bind(&bl);
  __ movl(pri, 1);
  __ bind(&done);
  __ addl(stk, 8);
  return true;
}

bool
Compiler::visitFLOAT_CMP_OP(CompareOp op)
{
  ConditionCode code;
  switch (op) {
  case CompareOp::Sgrtr:
    code = above;
    break;
  case CompareOp::Sgeq:
    code = above_equal;
    break;
  case CompareOp::Sleq:
    code = below_equal;
    break;
  case CompareOp::Sless:
    code = below;
    break;
  case CompareOp::Eq:
    code = equal;
    break;
  case CompareOp::Neq:
    code = not_equal;
    break;
  default:
    assert(false);
    reportError(SP_ERROR_INVALID_INSTRUCTION);
    return false;
  }
  emitFloatCmp(code);
  return true;
}

bool
Compiler::visitFLOAT_NOT()
{
  if (MacroAssembler::Features().sse) {
    __ xorps(xmm0, xmm0);
    __ ucomiss(Operand(stk, 0), xmm0);
  } else {
    __ fld32(Operand(stk, 0));
    __ fldz();
    __ fucomip(st1);
    __ fstp(st0);
  }

  // See emitFloatCmp() - this is a shorter version.
  Label done;
  __ movl(eax, 1);
  __ j(parity, &done);
  __ set(zero, r8_al);
  __ bind(&done);

  __ addl(stk, 4);
  return true;
}

bool
Compiler::visitSTACK(cell_t amount)
{
  __ addl(stk, amount);
  return true;
}

bool
Compiler::visitHEAP(cell_t amount)
{
  // Note: this must not clobber PRI.
  __ movl(alt, Operand(hpAddr()));
  __ addl(Operand(hpAddr()), amount);

  if (amount < 0) {
    __ cmpl(Operand(hpAddr()), context_->DataSize());
    jumpOnError(below, SP_ERROR_HEAPMIN);
  } else {
    __ movl(tmp, Operand(hpAddr()));
    __ lea(tmp, Operand(dat, ecx, NoScale, STACK_MARGIN));
    __ cmpl(tmp, stk);
    jumpOnError(above, SP_ERROR_HEAPLOW);
  }
  return true;
}

bool
Compiler::visitJUMP(cell_t offset)
{
  assert(block_->successors().size() == 1);

  Block* successor = block_->successors()[0];
  if (isNextBlock(successor)) {
    // We'll visit this block next, and this terminates the block, so there's
    // no need to emit a jump instruction.
    assert(!isBackedge(successor));
    return true;
  }

  Label* target = successor->label();
  if (isBackedge(successor)) {
    __ jmp32(target);
    backward_jumps_.push_back(BackwardJump(masm.pc(), op_cip_));
  } else {
    __ jmp(target);
  }
  return true;
}

bool
Compiler::visitJcmp(CompareOp op, cell_t offset)
{
  ConditionCode cc;
  switch (op) {
    case CompareOp::Zero:
    case CompareOp::NotZero:
      cc = (op == CompareOp::Zero) ? zero : not_zero;
      __ testl(pri, pri);
      break;
    case CompareOp::Eq:
    case CompareOp::Neq:
    case CompareOp::Sless:
    case CompareOp::Sleq:
    case CompareOp::Sgrtr:
    case CompareOp::Sgeq:
      cc = OpToCondition(op);
      __ cmpl(pri, alt);
      break;
    default:
      assert(false);
      return false;
  }

  assert(block_->successors().size() == 2);
  Block* fallthrough = block_->successors()[0];
  Block* target = block_->successors()[1];

  assert(!isBackedge(fallthrough));

  if (isBackedge(target)) {
    __ j32(cc, target->label());
    backward_jumps_.push_back(BackwardJump(masm.pc(), op_cip_));

    if (!isNextBlock(fallthrough))
      __ jmp(fallthrough->label());
    return true;
  }

  if (isNextBlock(target)) {
    // Invert the condition so we can fallthrough to the target instead.
    __ j(InvertConditionCode(cc), fallthrough->label());
  } else {
    __ j(cc, target->label());
    if (!isNextBlock(fallthrough))
      __ jmp(fallthrough->label());
  }
  return true;
}


bool
Compiler::visitTRACKER_PUSH_C(cell_t amount)
{
  __ push(pri);
  __ push(alt);

  __ push(amount);
  __ push(intptr_t(rt_->GetBaseContext()));
  __ callWithABI(ExternalAddress((void*)InvokePushTracker));
  __ addl(esp, 8);
  __ testl(eax, eax);
  jumpOnError(not_zero);

  __ pop(alt);
  __ pop(pri);
  return true;
}

bool
Compiler::visitTRACKER_POP_SETHEAP()
{
  // Save registers.
  __ subl(esp, 4);
  __ push(pri);
  __ push(alt);

  // Get the context pointer and call the sanity checker.
  __ push(intptr_t(rt_->GetBaseContext()));
  __ callWithABI(ExternalAddress((void*)InvokePopTrackerAndSetHeap));
  __ addl(esp, 4);
  __ testl(eax, eax);
  jumpOnError(not_zero);

  __ pop(alt);
  __ pop(pri);
  __ addl(esp, 4);
  return true;
}

bool
Compiler::visitINITARRAY(PawnReg reg, cell_t addr, cell_t iv_size, cell_t data_copy_size,
                         cell_t data_fill_size, cell_t fill_value)
{
  if (!iv_size) {
    // This is a flat array, we can inline something a little faster.
    __ push(edi);
    if (reg == PawnReg::Pri)
      __ lea(edi, Operand(dat, pri, NoScale));
    else
      __ lea(edi, Operand(dat, alt, NoScale));
    if (data_copy_size) {
      __ push(esi);
      __ lea(esi, Operand(dat, addr));
      __ cld();
      __ movl(ecx, data_copy_size);
      __ rep_movsd();
      __ pop(esi);
    }
    if (data_fill_size) {
      __ movl(eax, fill_value);
      __ movl(ecx, data_fill_size);
      __ rep_stosd();
    }
    __ pop(edi);
  } else {
    // Slow (multi-d) array initialization.
    // We need to sync |sp| first.
    __ subl(stk, dat);
    __ movl(Operand(spAddr()), stk);
    __ addl(stk, dat);

    __ push(alt);
    __ push(fill_value);
    __ push(data_fill_size);
    __ push(data_copy_size);
    __ push(iv_size);
    __ push(addr);
    if (reg == PawnReg::Pri)
      __ push(pri);
    else
      __ push(alt);
    __ push(intptr_t(rt_->GetBaseContext()));
    __ callWithABI(ExternalAddress((void*)InvokeInitArray));
    __ addl(esp, 7 * sizeof(intptr_t));
    __ pop(alt);
    __ testl(eax, eax);
    __ j(zero, &return_reported_error_);
  }
  return true;
}

bool
Compiler::visitBREAK()
{
  if (!Environment::get()->IsDebugBreakEnabled())
    return true;

  __ call(&debug_break_);
  emitCipMapping(op_cip_);
  return true;
}

bool
Compiler::visitHALT(cell_t value)
{
  // We don't support this. It's included in the bytestream by default, but it
  // must be unreachable.
  reportError(SP_ERROR_INVALID_INSTRUCTION);
  return false;
}

bool
Compiler::visitBOUNDS(uint32_t limit)
{
  OutOfBoundsErrorPath* bounds = new OutOfBoundsErrorPath(op_cip_, limit);
  ool_paths_.push_back(bounds);

  __ cmpl(eax, limit);
  __ j(above, bounds->label());
  return true;
}

void
Compiler::emitCheckAddress(Register reg)
{
  // Check if we're in memory bounds.
  __ cmpl(reg, context_->HeapSize());
  jumpOnError(not_below, SP_ERROR_MEMACCESS);

  // Check if we're in the invalid region between hp and sp.
  Label done;
  __ cmpl(reg, Operand(hpAddr()));
  __ j(below, &done);
  __ lea(tmp, Operand(dat, reg, NoScale));
  __ cmpl(tmp, stk);
  jumpOnError(below, SP_ERROR_MEMACCESS);
  __ bind(&done);
}

bool
Compiler::visitGENARRAY(uint32_t dims, bool autozero)
{
  if (dims == 1)
  {
    // flat array; we can generate this without indirection tables.
    // Note that we can overwrite ALT because technically STACK should be destroying ALT
    __ movl(alt, Operand(hpAddr()));
    __ movl(tmp, Operand(stk, 0));
    __ movl(Operand(stk, 0), alt);    // store base of the array into the stack.
    __ lea(alt, Operand(alt, tmp, ScaleFour));
    __ movl(Operand(hpAddr()), alt);
    __ addl(alt, dat);
    __ cmpl(alt, stk);
    jumpOnError(not_below, SP_ERROR_HEAPLOW);

    if (!rt_->UsesHeapScopes()) {
      __ shll(tmp, 2);
      __ subl(esp, 8);
      __ push(tmp);
      __ push(intptr_t(rt_->GetBaseContext()));
      __ callWithABI(ExternalAddress((void*)InvokePushTracker));
      __ movl(tmp, Operand(esp, 4));
      __ addl(esp, 16);
      __ shrl(tmp, 2);
      __ testl(eax, eax);
      jumpOnError(not_zero);
    }

    if (autozero) {
      // Note - tmp is ecx and still intact.
      __ push(eax);
      __ push(edi);
      __ xorl(eax, eax);
      __ movl(edi, Operand(stk, 0));
      __ addl(edi, dat);
      __ cld();
      __ rep_stosd();
      __ pop(edi);
      __ pop(eax);
    }
  } else {
    // We need to sync |sp| first.
    __ subl(stk, dat);
    __ movl(Operand(spAddr()), stk);
    __ addl(stk, dat);

    __ push(pri);
    __ subl(esp, 12);

    // int GenerateArray(cx, vars[], uint32_t, cell_t*, int, unsigned*);
    __ push(autozero ? 1 : 0);
    __ push(stk);
    __ push(dims);
    __ push(intptr_t(context_));
    __ callWithABI(ExternalAddress((void*)InvokeGenerateFullArray));
    __ addl(esp, 4 * sizeof(void*) + 12);

    // restore pri to tmp
    __ pop(tmp);

    __ testl(eax, eax);
    jumpOnError(not_zero);

    // Move tmp back to pri, remove pushed args.
    __ movl(pri, tmp);
    __ addl(stk, (dims - 1) * 4);
  }
  return true;
}

class CallThunk : public OutOfLinePath
{
 public:
  CallThunk(cell_t pcode_offset)
   : pcode_offset(pcode_offset)
  {
  }

  bool emit(Compiler* cc) override {
    cc->emitCallThunk(this);
    return true;
  }

  cell_t pcode_offset;
};

bool
Compiler::visitCALL(cell_t offset)
{
  RefPtr<MethodInfo> method = rt_->GetMethod(offset);
  if (!method || !method->jit()) {
    // Need to emit a delayed thunk.
    CallThunk* thunk = new CallThunk(offset);
    __ callWithABI(thunk->label());
    ool_paths_.push_back(thunk);
  } else {
    // Function is already emitted, we can do a direct call.
    __ callWithABI(ExternalAddress(method->jit()->GetEntryAddress()));
  }

  // Map the return address to the cip that started this call.
  emitCipMapping(op_cip_);
  return true;
}

void
Compiler::emitCallThunk(CallThunk* thunk)
{
  // Get the return address, since that is the call that we need to patch.
  __ movl(eax, Operand(esp, 0));

  // Enter the exit frame. This aligns the stack.
  __ enterExitFrame(ExitFrameType::Helper, 0);

  // We need to push 4 arguments, and one of them will need an extra word
  // on the stack. Allocate a big block so we're aligned.
  //
  // Note: we add 12 since the push above misaligned the stack.
  static const size_t kStackNeeded = 5 * sizeof(void*);
  static const size_t kStackReserve = ke::Align(kStackNeeded, 16);
  __ subl(esp, kStackReserve);

  // Set arguments.
  __ movl(Operand(esp, 3 * sizeof(void*)), eax);
  __ lea(edx, Operand(esp, 4 * sizeof(void*)));
  __ movl(Operand(esp, 2 * sizeof(void*)), edx);
  __ movl(Operand(esp, 1 * sizeof(void*)), intptr_t(thunk->pcode_offset));
  __ movl(Operand(esp, 0 * sizeof(void*)), intptr_t(context_));

  __ callWithABI(ExternalAddress((void*)CompileFromThunk));
  __ movl(edx, Operand(esp, 4 * sizeof(void*)));
  __ leaveExitFrame();

  __ testl(eax, eax);
  jumpOnError(not_zero);

  __ jmp(edx);
}

bool
Compiler::visitSYSREQ_N(uint32_t native_index, uint32_t nparams)
{
  NativeEntry* native = rt_->NativeAt(native_index);

  // Store the number of parameters on the stack.
  __ movl(Operand(stk, -4), nparams);
  __ subl(stk, 4);
  emitLegacyNativeCall(native_index, native);
  __ addl(stk, (nparams + 1) * sizeof(cell_t));
  return true;
}

bool
Compiler::visitSYSREQ_C(uint32_t native_index)
{
  emitLegacyNativeCall(native_index, rt_->NativeAt(native_index));
  return true;
}

static cell_t NativeInvokeThunk(NativeEntry* native, IPluginContext* ctx, const cell_t* params)
{
  if (native->legacy_fn)
    return native->legacy_fn(ctx, params);
  return native->callback->Invoke(ctx, params);
} 

void
Compiler::emitLegacyNativeCall(uint32_t native_index, NativeEntry* native)
{
  CodeLabel return_address;
  __ pushInlineExitFrame(ExitFrameType::Native, native_index, &return_address);

  // Save registers.
  __ push(edx);

  // Check whether the native is bound.
  bool immutable = native->status == SP_NATIVE_BOUND &&
                   !(native->flags & (SP_NTVFLAG_EPHEMERAL|SP_NTVFLAG_OPTIONAL));
  if (!immutable) {
    __ movl(edx, Operand(ExternalAddress(&native->status)));
    __ cmpl(edx, SP_NATIVE_BOUND);
    __ j(not_equal, &unbound_native_error_);
  }

  bool fast_path = immutable && native->legacy_fn;

  // If we're going to take the slow path, the stack has an extra word, so we
  // need to align it here.
  if (!fast_path)
    __ subl(esp, 12);

  // Save the old heap pointer.
  __ push(Operand(hpAddr()));

  // Push the last parameter for the C++ function.
  __ push(stk);

  // Relocate our absolute stk to be dat-relative, and update the context's
  // view.
  __ subl(stk, dat);
  __ movl(Operand(spAddr()), stk);

  // Push the first parameter, the context.
  __ push(intptr_t(rt_->GetBaseContext()));

  if (fast_path) {
    // Fast invoke, skip right to the function call.
    //
    // Stack (16 bytes):
    //   12: Saved EDX
    //    8: Saved HP
    //    4: Cells
    //    0: Context
    __ callWithABI(ExternalAddress((void*)native->legacy_fn));
  } else {
    // Slower invoke, go through a wrapper so we don't have to make this super
    // complicated handling all the different calling conventions.
    //
    // Stack (32 bytes):
    //   28: Saved EDX
    //   24: Alignment (3 words)
    //   12: Saved HP
    //    8: Cells
    //    4: Context
    //    0: Native
    __ push(reinterpret_cast<intptr_t>(native));
    __ callWithABI(ExternalAddress((void*)NativeInvokeThunk));
  }
  __ bind(&return_address);
  // Map the return address to the cip that initiated this call.
  emitCipMapping(op_cip_);

  // Restore the heap pointer.
  __ movl(edx, Operand(esp, (fast_path ? 2 : 3) * sizeof(intptr_t)));
  __ movl(Operand(hpAddr()), edx);

  // Restore ALT.
  __ movl(edx, Operand(esp, (fast_path ? 3 : 7) * sizeof(intptr_t)));

  // Restore SP.
  __ addl(stk, dat);

  // Remove the inline frame, + our four arguments.
  __ popInlineExitFrame(fast_path ? 4 : 8);

  // Check for errors. Note we jump directly to the return stub since the
  // error has already been reported.
  ExternalAddress exn_code(Environment::get()->addressOfExceptionCode());
  __ cmpl(Operand(exn_code), 0);
  __ j(not_zero, &return_reported_error_);
}

bool
Compiler::visitSWITCH(cell_t defaultOffset,
                      const CaseTableEntry* cases,
                      size_t ncases)
{
  assert(block_->successors().size() == ncases + 1);
  Block* defaultCase = block_->successors()[0];

  // Degenerate - 0 cases.
  if (!ncases) {
    if (!isNextBlock(defaultCase))
      __ jmp(defaultCase->label());
    return true;
  }

  // Degenerate - 1 case.
  if (ncases == 1) {
    Block* maybe = block_->successors()[1];
    __ cmpl(pri, cases[0].value);
    __ j(equal, maybe->label());
    if (!isNextBlock(defaultCase))
      __ jmp(defaultCase->label());
    return true;
  }

  // We have two or more cases, so let's generate a full switch. Decide
  // whether we'll make an if chain, or a jump table, based on whether
  // the numbers are strictly sequential.
  bool sequential = true;
  {
    cell_t first = cases[0].value;
    cell_t last = first;
    for (size_t i = 1; i < ncases; i++) {
      if (cases[i].value != ++last) {
        sequential = false;
        break;
      }
    }
  }

  // First check whether the bounds are correct: if (a < LOW || a > HIGH);
  // this check is valid whether or not we emit a sequential-optimized switch.
  cell_t low = cases[0].value;
  if (low != 0) {
    // negate it so we'll get a lower bound of 0.
    low = -low;
    __ lea(tmp, Operand(pri, low));
  } else {
    __ movl(tmp, pri);
  }

  cell_t high = abs(cases[0].value - cases[ncases - 1].value);
  __ cmpl(tmp, high);
  __ j(above, defaultCase->label());

  if (sequential) {
    // Optimized table version. The tomfoolery below is because we only have
    // one free register... it seems unlikely pri or alt will be used given
    // that we're at the end of a control-flow point, but we'll play it safe.
    CodeLabel table;
    __ push(eax);
    __ movl(eax, &table);
    __ movl(ecx, Operand(eax, ecx, ScaleFour));
    __ pop(eax);
    __ jmp(ecx);

    __ bind(&table);
    for (size_t i = 0; i < ncases; i++) {
      Block* target = block_->successors()[i + 1];
      __ emit_absolute_address(target->label());
    }
  } else {
    // Slower version. Go through each case and generate a check.
    for (size_t i = 0; i < ncases; i++) {
      Block* target = block_->successors()[i + 1];
      __ cmpl(pri, cases[i].value);
      __ j(equal, target->label());
    }
    __ jmp(defaultCase->label());
  }
  return true;
}

bool
Compiler::visitHEAP_SAVE()
{
  // Allocate one cell on the heap.
  visitHEAP(sizeof(cell_t));
  // Get the addres of the old heap scope in pri.
  __ movl(pri, Operand(hpScopeAddr()));
  // Store the old heap scope address into the new heap scope.
  __ movl(Operand(dat, alt, NoScale), pri);
  // Update the context's current heap scope.
  __ movl(Operand(hpScopeAddr()), alt);
  return true;
}

bool
Compiler::visitHEAP_RESTORE()
{
  // Get the current heap scope address.
  __ movl(ecx, Operand(hpScopeAddr()));
  // Get the previous heap scope address.
  __ movl(alt, Operand(dat, ecx, NoScale));
  // Update the heap pointer.
  __ movl(Operand(hpAddr()), ecx);
  // Update the heap scope.
  __ movl(Operand(hpScopeAddr()), alt);
  return true;
}

void
Compiler::emitFloatCmp(ConditionCode cc)
{
  unsigned lhs = 4;
  unsigned rhs = 0;
  if (cc == below || cc == below_equal) {
    // NaN results in ZF=1 PF=1 CF=1
    //
    // ja/jae check for ZF,CF=0 and CF=0. If we make all relational compares
    // look like ja/jae, we'll guarantee all NaN comparisons will fail (which
    // would not be true for jb/jbe, unless we checked with jp).
    if (cc == below)
      cc = above;
    else
      cc = above_equal;
    rhs = 4;
    lhs = 0;
  }

  if (MacroAssembler::Features().sse) {
    __ movss(xmm0, Operand(stk, rhs));
    __ ucomiss(Operand(stk, lhs), xmm0);
  } else {
    __ fld32(Operand(stk, rhs));
    __ fld32(Operand(stk, lhs));
    __ fucomip(st1);
    __ fstp(st0);
  }

  // An equal or not-equal needs special handling for the parity bit.
  if (cc == equal || cc == not_equal) {
    // If NaN, PF=1, ZF=1, and E/Z tests ZF=1.
    //
    // If NaN, PF=1, ZF=1 and NE/NZ tests Z=0. But, we want any != with NaNs
    // to return true, including NaN != NaN.
    //
    // To make checks simpler, we set |eax| to the expected value of a NaN
    // beforehand. This also clears the top bits of |eax| for setcc.
    Label done;
    __ movl(eax, (cc == equal) ? 0 : 1);
    __ j(parity, &done);
    __ set(cc, r8_al);
    __ bind(&done);
  } else {
    __ movl(eax, 0);
    __ set(cc, r8_al);
  }
  __ addl(stk, 8);
}

void
Compiler::jumpOnError(ConditionCode cc, int err)
{
  // Note: we accept 0 for err. In this case we expect the error to be in eax.
  ErrorPath* path = new ErrorPath(op_cip_, err);
  ool_paths_.push_back(path);

  __ j(cc, path->label());
}

void
Compiler::emitOutOfBoundsErrorPath(OutOfBoundsErrorPath* path)
{
  CodeLabel return_address;
  __ alignStack();
  __ pushInlineExitFrame(ExitFrameType::Helper, 0, &return_address);
  __ subl(esp, 8);
  __ push(path->bounds);
  __ push(eax);
  __ callWithABI(ExternalAddress((void*)ReportOutOfBoundsError));
  __ bind(&return_address);
  emitCipMapping(path->cip);
  __ popInlineExitFrame(4);
  __ jmp(&return_reported_error_);
}

void
Compiler::emitErrorHandlers()
{
  Label return_to_invoke;

  if (report_error_.used()) {
    __ bind(&report_error_);

    // Create the exit frame. We always get here through a call from the opcode
    // (and always via an out-of-line thunk).
    __ enterExitFrame(ExitFrameType::Helper, 0);

    // Align the stack and call.
    __ subl(esp, 12);
    __ push(eax);
    __ callWithABI(ExternalAddress((void*)InvokeReportError));
    __ leaveExitFrame();
    __ jmp(&return_to_invoke);
  }

  // The unbound native path re-uses the native exit frame so the stack trace
  // looks as if the native was bound.
  if (unbound_native_error_.used()) {
    __ bind(&unbound_native_error_);
    __ alignStack();
    __ callWithABI(ExternalAddress((void*)ReportUnboundNative));
    __ jmp(&return_reported_error_);
  }

  // The timeout uses a special stub.
  if (throw_timeout_.used()) {
    __ bind(&throw_timeout_);

    // Create the exit frame.
    __ enterExitFrame(ExitFrameType::Helper, 0);

    // Since the return stub wipes out the stack, we don't need to addl after
    // the call.
    __ callWithABI(ExternalAddress((void*)InvokeReportTimeout));
    __ leaveExitFrame();
    __ jmp(&return_reported_error_);
  }

  // We get here if we know an exception is already pending.
  if (return_reported_error_.used()) {
    __ bind(&return_reported_error_);
    __ call(&return_to_invoke);
  }

  if (return_to_invoke.used()) {
    __ bind(&return_to_invoke);

    // We get here either through an explicit call, or a call that terminated
    // in a tail-jmp here.
    __ enterExitFrame(ExitFrameType::Helper, 0);

    // We cannot jump to the return stub just yet. We could be multiple frames
    // deep, and our |ebp| does not match the initial frame. Find and restore
    // it now.
    __ callWithABI(ExternalAddress((void*)find_entry_fp));
    __ leaveExitFrame();

    __ movl(ebp, eax);
    __ jmp(ExternalAddress(env_->stubs()->ReturnStub()));
  }
}

void
Compiler::emitThrowPath(int err)
{
  __ movl(eax, err);
  __ jmp(&report_error_);
}

void
Compiler::emitDebugBreakHandler()
{
  // Common path for invoking debugger.
  __ bind(&debug_break_);

  // Get and store the current stack pointer.
  __ movl(tmp, stk);
  __ subl(tmp, dat);
  __ movl(Operand(spAddr()), tmp);

  // Enter the exit frame. This aligns the stack.
  __ enterExitFrame(ExitFrameType::Helper, 0);

  // Allocate enough memory to keep the stack aligned.
  static const size_t kStackNeeded = 2 * sizeof(void *);
  static const size_t kStackReserve = ke::Align(kStackNeeded, 16);
  __ subl(esp, kStackReserve);

  // Get the context pointer and call the debugging break handler.
  __ movl(Operand(esp, 1 * sizeof(void *)), 0); // IErrorReport*
  __ movl(Operand(esp, 0 * sizeof(void *)), intptr_t(rt_->GetBaseContext()));
  __ call(ExternalAddress((void *)InvokeDebugger));
  __ leaveExitFrame();
  __ testl(eax, eax);
  jumpOnError(not_zero);
  __ ret();
}

void
CompilerBase::PatchCallThunk(uint8_t* pc, void* target)
{
  *(intptr_t*)(pc - 4) = intptr_t(target) - intptr_t(pc);
}

} // namespace sp
