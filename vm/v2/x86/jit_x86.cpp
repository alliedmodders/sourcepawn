/**
 * vim: set ts=4 sw=4 tw=99 et:
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
 */

#include "jit_x86.h"
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "code-stubs.h"
#include "debugging.h"
#include "environment.h"
#include "x86/features-x86.h"
#include "x86/frames-x86.h"
#include "linking.h"
#include "v2/method-info.h"
#include "v2/runtime.h"
#include "v2/runtime-helpers.h"
#include "watchdog_timer.h"

#define __ masm.

namespace sp::v2 {

static inline ConditionCode
OpToCondition(CompareOp op) {
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

Compiler::Compiler(Runtime* rt, MethodInfo* method)
 : CompilerBase(rt, method) {
}

Compiler::~Compiler() {
}

// No exit frame - error code is returned directly.
static int
InvokeGenerateFullArray(Runtime* cx, uint32_t argc, cell_t* argv, int autozero) {
    return cx->generateFullArray(argc, argv, autozero);
}

bool
Compiler::visitMOVE(PawnReg reg) {
    if (reg == PawnReg::Pri)
        __ movl(pri, alt);
    else
        __ movl(alt, pri);
    return true;
}

bool
Compiler::visitXCHG() {
    __ xchgl(pri, alt);
    return true;
}


bool
Compiler::visitPUSH(PawnReg src) {
    Register reg = (src == PawnReg::Pri) ? pri : alt;
    __ movl(Operand(stk, -4), reg);
    __ subl(stk, 4);
    return true;
}

bool Compiler::visitPUSH_C(cell_t value) {
    __ movl(Operand(stk, -4), value);
    __ subl(stk, 4);
    return true;
}

bool Compiler::visitPUSH_ADR(cell_t slot) {
    // We temporarily relocate FRM to be a local address instead of an
    // absolute address.
    __ movl(tmp, Operand(frmAddr()));
    __ lea(tmp, Operand(tmp, StackOffset(slot)));
    __ movl(Operand(stk, -4), tmp);
    __ subl(stk, 4);
    return true;
}

bool Compiler::visitPUSH_S(cell_t slot) {
    __ movl(tmp, Operand(frm, StackOffset(slot)));
    __ movl(Operand(stk, -4), tmp);
    __ subl(stk, 4);
    return true;
}

bool
Compiler::visitZERO(PawnReg dest) {
    Register reg = (dest == PawnReg::Pri) ? pri : alt;
    __ xorl(reg, reg);
    return true;
}

bool
Compiler::visitADD() {
    __ addl(pri, alt);
    return true;
}

bool
Compiler::visitSUB_ALT() {
    __ movl(tmp, alt);
    __ subl(tmp, pri);
    __ movl(pri, tmp);
    return true;
}

void
Compiler::emitPrologue() {
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

    if (cell_t stack_needed = method_info_->StackSizeForLocalSlots())
        __ addl(stk, stack_needed);
}

bool
Compiler::visitSHL() {
    __ movl(ecx, alt);
    __ shll_cl(pri);
    return true;
}

bool
Compiler::visitSHR() {
    __ movl(ecx, alt);
    __ shrl_cl(pri);
    return true;
}

bool
Compiler::visitSSHR() {
    __ movl(ecx, alt);
    __ sarl_cl(pri);
    return true;
}

bool
Compiler::visitSMUL() {
    __ imull(pri, alt);
    return true;
}

bool
Compiler::visitNOT() {
    __ testl(eax, eax);
    __ movl(eax, 0);
    __ set(zero, r8_al);
    return true;
}

bool
Compiler::visitNEG() {
    __ negl(eax);
    return true;
}

bool
Compiler::visitXOR() {
    __ xorl(pri, alt);
    return true;
}

bool
Compiler::visitOR() {
    __ orl(pri, alt);
    return true;
}

bool
Compiler::visitAND() {
    __ andl(pri, alt);
    return true;
}

bool
Compiler::visitINVERT() {
    __ notl(pri);
    return true;
}


bool
Compiler::visitSMUL_C(cell_t value) {
    __ imull(pri, pri, value);
    return true;
}

bool
Compiler::visitCompareOp(CompareOp op) {
    ConditionCode cc = OpToCondition(op);
    __ cmpl(pri, alt);
    __ movl(pri, 0);
    __ set(cc, r8_al);
    return true;
}

bool
Compiler::visitINC_PRI() {
    __ addl(pri, 1);
    return true;
}

bool
Compiler::visitDEC_PRI() {
    __ subl(pri, 1);
    return true;
}

bool
Compiler::visitLOAD_PRI(cell_t srcaddr) {
    __ movl(pri, Operand(dat, srcaddr));
    return true;
}

bool
Compiler::visitLOAD_S(PawnReg dest, cell_t srcoffs) {
    Register reg = (dest == PawnReg::Pri) ? pri : alt;
    __ movl(reg, Operand(frm, StackOffset(srcoffs)));
    return true;
}

bool
Compiler::visitLREF_S_PRI(cell_t srcoffs) {
    __ movl(pri, Operand(frm, StackOffset(srcoffs)));
    __ movl(pri, Operand(dat, pri, NoScale));
    return true;
}

bool
Compiler::visitCONST(PawnReg dest, cell_t val) {
    Register reg = (dest == PawnReg::Pri) ? pri : alt;
    __ movl(reg, val);
    return true;
}

bool
Compiler::visitADDR(PawnReg dest, cell_t offset) {
    Register reg = (dest == PawnReg::Pri) ? pri : alt;
    __ movl(reg, Operand(frmAddr()));
    __ addl(reg, StackOffset(offset));
    return true;
}

bool
Compiler::visitSTOR_PRI(cell_t offset) {
    __ movl(Operand(dat, offset), pri);
    return true;
}

bool
Compiler::visitSTOR_S(cell_t offset, PawnReg src) {
    Register reg = (src == PawnReg::Pri) ? pri : alt;
    __ movl(Operand(frm, StackOffset(offset)), reg);
    return true;
}

bool
Compiler::visitIDXADDR() {
    __ lea(pri, Operand(alt, pri, ScaleFour));
    return true;
}


bool
Compiler::visitPOP(PawnReg dest) {
    Register reg = (dest == PawnReg::Pri) ? pri : alt;
    __ movl(reg, Operand(stk, 0));
    __ addl(stk, 4);
    return true;
}

bool
Compiler::visitSWAP_ALT() {
    __ movl(tmp, Operand(stk, 0));
    __ movl(Operand(stk, 0), alt);
    __ movl(alt, tmp);
    return true;
}

bool
Compiler::visitLOAD_I() {
    emitCheckAddress(pri);
    __ movl(pri, Operand(dat, pri, NoScale));
    return true;
}

bool
Compiler::visitSTOR_I() {
    emitCheckAddress(alt);
    __ movl(Operand(dat, alt, NoScale), pri);
    return true;
}

bool
Compiler::visitSDIV_ALT_I32() {
    Register dividend = alt;
    Register divisor = pri;

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
    __ movl(eax, dividend);
    __ sarl(edx, 31);
    __ idivl(tmp);
    return true;
}

bool
Compiler::visitSMOD_ALT_I32() {
    visitSDIV_ALT_I32();
    __ movl(eax, edx);
    return true;
}

bool
Compiler::visitLODB_I() {
    emitCheckAddress(pri);
    __ movl(pri, Operand(dat, pri, NoScale));
    __ andl(pri, 0xff);
    return true;
}

bool
Compiler::visitSTRB_I() {
    emitCheckAddress(alt);
    __ movb(Operand(dat, alt, NoScale), pri);
    return true;
}

bool
Compiler::visitRETN() {
    for (uint32_t i = 0; i < block_->heap_scope_depth(); i++)
        visitHEAP_RESTORE();

    // Restore the old stack and frame pointer.
    __ movl(stk, frm);
    __ movl(frm, Operand(stk, 4)); // get the old frm
    __ movl(tmp, Operand(stk, 0)); // get the old hp
    __ movl(Operand(hpAddr()), tmp);
    __ addl(stk, 8);                  // pop stack
    __ movl(Operand(frmAddr()), frm); // store back old frm
    __ addl(frm, dat);                // relocate

    // Remove parameters.
    __ movl(tmp, Operand(stk, 0));
    __ lea(stk, Operand(stk, tmp, ScaleFour, 4));

    __ leaveFrame();
    __ ret();
    return true;
}

bool
Compiler::visitMOVS(uint32_t amount) {
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
Compiler::visitFILL(uint32_t amount) {
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
Compiler::visitSTRADJUST_PRI() {
    __ addl(pri, 4);
    __ sarl(pri, 2);
    return true;
}

ConditionCode ToFloatConditionCode(CompareOp op) {
    switch (op) {
        case CompareOp::Sgrtr:
            return above;
        case CompareOp::Sgeq:
            return above_equal;
        case CompareOp::Sleq:
            return below_equal;
        case CompareOp::Sless:
            return below;
        case CompareOp::Eq:
            return equal;
        case CompareOp::Neq:
            return not_equal;
        default:
            assert(false);
            return zero;
    }
}

bool
Compiler::visitHEAP(cell_t amount) {
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
Compiler::visitJcmp(CompareOp op, cell_t offset) {
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
Compiler::visitINITARRAY_ALT(cell_t addr, cell_t iv_size, cell_t data_copy_size,
                             cell_t data_fill_size, cell_t fill_value) {
    if (!iv_size) {
        // This is a flat array, we can inline something a little faster.
        __ push(edi);
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
        __ push(alt);
        __ push(intptr_t(rt_->GetBaseContext()));
        // :TODO: this needs an exit frame!
        __ callWithABI(ExternalAddress((void*)InvokeInitArray));
        __ addl(esp, 7 * sizeof(intptr_t));
        __ pop(alt);
        __ testl(eax, eax);
        __ j(zero, &return_reported_error_);
    }
    return true;
}

bool
Compiler::visitBREAK() {
    if (!Environment::get()->IsDebugBreakEnabled())
        return true;

    __ call(&debug_break_);
    emitCipMapping(op_cip_);
    return true;
}

bool
Compiler::visitBOUNDS(uint32_t limit) {
    OutOfBoundsError error(op_cip_, limit);

    __ cmpl(eax, limit);
    __ j(above, &error.label);

    bounds_errors_.emplace_back(std::move(error));
    return true;
}

void
Compiler::emitCheckAddress(Register reg, size_t read_size) {
    // Check if we're in memory bounds.
    __ cmpl(reg, context_->HeapSize() - read_size + 1);
    jumpOnError(above_equal, SP_ERROR_MEMACCESS);

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
Compiler::visitGENARRAY(uint32_t dims, bool autozero) {
    if (dims == 1) {
        // flat array; we can generate this without indirection tables.
        // Note that we can overwrite ALT because technically STACK should be destroying ALT
        __ movl(alt, Operand(hpAddr()));
        __ movl(tmp, Operand(stk, 0));
        __ movl(Operand(stk, 0), alt); // store base of the array into the stack.
        __ lea(alt, Operand(alt, tmp, ScaleFour));
        __ movl(Operand(hpAddr()), alt);
        __ addl(alt, dat);
        __ cmpl(alt, stk);
        jumpOnError(not_below, SP_ERROR_HEAPLOW);

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

bool
Compiler::visitCALL(cell_t offset) {
    RefPtr<BaseMethodInfo> method = rt_->GetMethod(offset);
    if (!method || !method->jit()) {
        // Need to emit a delayed thunk.
        CallThunk thunk(offset);
        __ callWithABI(&thunk.label);
        call_thunks_.emplace_back(std::move(thunk));
    } else {
        // Function is already emitted, we can do a direct call.
        __ callWithABI(ExternalAddress(method->jit()->GetEntryAddress()));
    }

    // Map the return address to the cip that started this call.
    emitCipMapping(op_cip_);
    return true;
}

void
Compiler::emitCallThunk(CallThunk* thunk) {
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
Compiler::visitSYSREQ_N(uint32_t native_index, uint32_t nparams) {
    NativeEntry* native = rt_->NativeAt(native_index);

    // Store the number of parameters on the stack.
    __ movl(Operand(stk, -4), nparams);
    __ subl(stk, 4);
    emitLegacyNativeCall(native_index, native);
    __ addl(stk, (nparams + 1) * sizeof(cell_t));
    return true;
}

void
Compiler::emitLegacyNativeCall(uint32_t native_index, NativeEntry* native) {
    CodeLabel return_address;
    __ pushInlineExitFrame(ExitFrameType::Native, native_index, &return_address);

    // Save registers.
    __ push(edx);

    // Check whether the native is bound.
    bool immutable = native->status == SP_NATIVE_BOUND &&
                     !(native->flags & (SP_NTVFLAG_EPHEMERAL | SP_NTVFLAG_OPTIONAL));
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

    if (fast_path) {
        // Fast invoke, skip right to the function call.
        //
        // Stack (16 bytes):
        //   12: Saved EDX
        //    8: Saved HP
        //    4: Cells
        //    0: Context
        __ push(intptr_t(rt_->GetBaseContext()));
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
        __ push(intptr_t(rt_->GetBaseContext()));
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
Compiler::visitSWITCH(cell_t defaultOffset, const CaseTableEntry* cases, size_t ncases) {
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

    cell_t low = cases[0].value;
    if (low != INT_MIN && sequential) {
        // First check whether the bounds are correct: if (a < LOW || a > HIGH);
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
Compiler::visitHEAP_SAVE() {
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
Compiler::visitHEAP_RESTORE() {
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
Compiler::emitFloatCmp(ConditionCode cc) {
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

    __ movss(xmm0, Operand(stk, rhs));
    __ ucomiss(Operand(stk, lhs), xmm0);

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

bool
Compiler::visitMOVE_I64() {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ movl(tmp, Operand(dat, pri, NoScale, 0));
    __ movl(Operand(dat, alt, NoScale, 0), tmp);
    __ movl(tmp, Operand(dat, pri, NoScale, 4));
    __ movl(Operand(dat, alt, NoScale, 4), tmp);
    return true;
}

bool
Compiler::visitCVT_I64(cell_t slot) {
    __ movl(tmp, alt);
    __ cdq();
    __ movl(Operand(frm, StackOffset(slot)), eax);
    __ movl(Operand(frm, StackOffset(slot) + 4), edx);
    __ movl(alt, tmp);

    __ movl(pri, Operand(frmAddr()));
    __ addl(pri, StackOffset(slot));
    return true;
}

bool
Compiler::visitTRUNCATE_I64() {
    emitCheckAddress(pri, sizeof(int64_t));
    __ movl(pri, Operand(dat, pri, NoScale, 0));
    return true;
}

bool
Compiler::visitTEST_I64() {
    emitCheckAddress(pri, sizeof(int64_t));
    __ movl(ecx, Operand(dat, pri, NoScale, 0));
    __ orl(ecx, Operand(dat, pri, NoScale, 0));
    __ set(not_equal, ecx);
    __ movzxb(pri, ecx);
    return true;
}

bool
Compiler::visitINVERT_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    __ movl(ecx, Operand(dat, pri, NoScale, 0));
    __ notl(ecx);
    __ movl(Operand(frm, StackOffset(slot)), ecx);
    __ movl(ecx, Operand(dat, pri, NoScale, 4));
    __ notl(ecx);
    __ movl(Operand(frm, StackOffset(slot) + 4), ecx);

    __ movl(pri, Operand(frmAddr()));
    __ addl(pri, StackOffset(slot));
    return true;
}

bool
Compiler::visitNEG_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    __ movl(ecx, Operand(dat, pri, NoScale, 0));
    __ movl(Operand(frm, StackOffset(slot)), ecx);
    __ movl(ecx, Operand(dat, pri, NoScale, 4));
    __ movl(Operand(frm, StackOffset(slot) + 4), ecx);

    __ xorl(ecx, ecx);
    __ negl(Operand(frm, StackOffset(slot)));
    __ sbbl(ecx, Operand(frm, StackOffset(slot) + 4));
    __ movl(Operand(frm, StackOffset(slot) + 4), ecx);

    __ movl(pri, Operand(frmAddr()));
    __ addl(pri, StackOffset(slot));
    return true;
}

bool
Compiler::visitSMUL_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ push(ebx);
    __ movl(ebx, Operand(dat, pri, NoScale, 4)); // clobbers frm
    __ movl(ecx, Operand(dat, alt, NoScale, 4));
    __ imull(ebx, Operand(dat, alt, NoScale, 0));
    __ imull(ecx, Operand(dat, pri, NoScale, 0));
    __ movl(eax, Operand(dat, pri, NoScale, 0)); // clobbers pri
    __ mull(Operand(dat, alt, NoScale, 0));      // output in eax
    __ addl(ecx, ebx);
    __ addl(edx, ecx);
    __ pop(ebx);

    __ movl(Operand(frm, StackOffset(slot)), eax);
    __ movl(Operand(frm, StackOffset(slot) + 4), edx);

    __ movl(pri, Operand(frmAddr()));
    __ addl(pri, StackOffset(slot));
    return true;
}

// This is not trivially possible to inline, so we need to call a helper.
bool
Compiler::visitSDIV_ALT_I64(cell_t pri_slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    static const size_t kStackNeeded = 3 * sizeof(void*);
    static const size_t kStackReserve = ke::Align(kStackNeeded, 16);
    __ subl(esp, kStackReserve);

    __ lea(ecx, Operand(frm, StackOffset(pri_slot)));
    __ movl(Operand(esp, 2 * sizeof(void*)), ecx);

    __ lea(alt, Operand(dat, alt, NoScale, 0));
    __ movl(Operand(esp, 1 * sizeof(void*)), alt);
    __ lea(pri, Operand(dat, pri, NoScale, 0));
    __ movl(Operand(esp, 0 * sizeof(void*)), pri);
    __ callWithABI(ExternalAddress((void*)Int64Div));
    __ addl(esp, kStackReserve);
    __ testl(eax, eax);
    jumpOnError(not_zero);

    __ movl(pri, Operand(frmAddr()));
    __ addl(pri, StackOffset(pri_slot));
    return true;
}

bool
Compiler::visitSMOD_ALT_I64(cell_t pri_slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    static const size_t kStackNeeded = 3 * sizeof(void*);
    static const size_t kStackReserve = ke::Align(kStackNeeded, 16);
    __ subl(esp, kStackReserve);

    __ lea(ecx, Operand(frm, StackOffset(pri_slot)));
    __ movl(Operand(esp, 2 * sizeof(void*)), ecx);

    __ lea(alt, Operand(dat, alt, NoScale, 0));
    __ movl(Operand(esp, 1 * sizeof(void*)), alt);
    __ lea(pri, Operand(dat, pri, NoScale, 0));
    __ movl(Operand(esp, 0 * sizeof(void*)), pri);
    __ callWithABI(ExternalAddress((void*)Int64Mod));
    __ addl(esp, kStackReserve);
    __ testl(eax, eax);
    jumpOnError(not_zero);

    __ movl(pri, Operand(frmAddr()));
    __ addl(pri, StackOffset(pri_slot));
    return true;
}

bool
Compiler::visitADD_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ movq(xmm1, Operand(dat, pri, NoScale, 0));
    __ movq(xmm0, Operand(dat, alt, NoScale, 0));
    __ paddq(xmm0, xmm1);
    __ movq(Operand(frm, StackOffset(slot)), xmm0);

    __ movl(pri, Operand(frmAddr()));
    __ addl(pri, StackOffset(slot));
    return true;
}

bool
Compiler::visitSUB_ALT_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ movq(xmm1, Operand(dat, pri, NoScale, 0));
    __ movq(xmm0, Operand(dat, alt, NoScale, 0));
    __ psubq(xmm0, xmm1);
    __ movq(Operand(frm, StackOffset(slot)), xmm0);

    __ movl(pri, Operand(frmAddr()));
    __ addl(pri, StackOffset(slot));
    return true;
}

bool
Compiler::visitSHL_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ lea(ecx, Operand(dat, alt, NoScale, 0));
    __ push(ecx);
    __ lea(ecx, Operand(dat, pri, NoScale, 0));
    __ push(ecx);

    Label done;
    __ movl(eax, Operand(esp, 4));
    __ movl(ecx, Operand(eax, 0)); // low 32-bits of alt
    __ movl(eax, Operand(esp, 0));
    __ movl(edx, Operand(eax, 4)); // hi 32-bits of pri
    __ movl(eax, Operand(eax, 0)); // lo 32-bits of pri
    __ shld(edx, eax);
    __ shll_cl(eax);
    __ testb(ecx, 0x20);
    __ j(equal, &done);
    __ movl(edx, eax);
    __ xorl(eax, eax);
    __ bind(&done);
    __ addl(esp, 8);

    __ movl(Operand(frm, StackOffset(slot)), eax);
    __ movl(Operand(frm, StackOffset(slot) + 4), edx);

    __ movl(pri, Operand(frmAddr()));
    __ addl(pri, StackOffset(slot));
    return true;
}

bool
Compiler::visitSSHR_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ lea(ecx, Operand(dat, alt, NoScale, 0));
    __ push(ecx);
    __ lea(ecx, Operand(dat, pri, NoScale, 0));
    __ push(ecx);

    Label done;
    __ movl(eax, Operand(esp, 4));
    __ movl(ecx, Operand(eax, 0)); // low 32-bits of alt
    __ movl(eax, Operand(esp, 0));
    __ movl(edx, Operand(eax, 4)); // hi 32-bits of pri
    __ movl(eax, Operand(eax, 0)); // lo 32-bits of pri
    __ shrd(eax, edx);
    __ sarl_cl(edx);
    __ testb(ecx, 0x20);
    __ j(equal, &done);
    __ movl(eax, edx);
    __ sarl(edx, 0x1f);
    __ bind(&done);
    __ addl(esp, 8);

    __ movl(Operand(frm, StackOffset(slot)), eax);
    __ movl(Operand(frm, StackOffset(slot) + 4), edx);

    __ movl(pri, Operand(frmAddr()));
    __ addl(pri, StackOffset(slot));
    return true;
}

bool
Compiler::visitSHR_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ lea(ecx, Operand(dat, alt, NoScale, 0));
    __ push(ecx);
    __ lea(ecx, Operand(dat, pri, NoScale, 0));
    __ push(ecx);

    Label done;
    __ movl(eax, Operand(esp, 4));
    __ movl(ecx, Operand(eax, 0)); // low 32-bits of alt
    __ movl(eax, Operand(esp, 0));
    __ movl(edx, Operand(eax, 4)); // hi 32-bits of pri
    __ movl(eax, Operand(eax, 0)); // lo 32-bits of pri
    __ shrd(eax, edx);
    __ shrl_cl(edx);
    __ testb(ecx, 0x20);
    __ j(equal, &done);
    __ movl(eax, edx);
    __ xorl(edx, edx);
    __ bind(&done);
    __ addl(esp, 8);

    __ movl(Operand(frm, StackOffset(slot)), eax);
    __ movl(Operand(frm, StackOffset(slot) + 4), edx);

    __ movl(pri, Operand(frmAddr()));
    __ addl(pri, StackOffset(slot));
    return true;
}

bool
Compiler::visitOR_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));
    __ movl(ecx, Operand(dat, pri, NoScale, 0));
    __ orl(ecx, Operand(dat, alt, NoScale, 0));
    __ movl(Operand(frm, StackOffset(slot)), ecx);
    __ movl(ecx, Operand(dat, pri, NoScale, 4));
    __ orl(ecx, Operand(dat, alt, NoScale, 4));
    __ movl(Operand(frm, StackOffset(slot) + 4), ecx);

    __ movl(pri, Operand(frmAddr()));
    __ addl(pri, StackOffset(slot));
    return true;
}

bool
Compiler::visitAND_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));
    __ movl(ecx, Operand(dat, pri, NoScale, 0));
    __ andl(ecx, Operand(dat, alt, NoScale, 0));
    __ movl(Operand(frm, StackOffset(slot)), ecx);
    __ movl(ecx, Operand(dat, pri, NoScale, 4));
    __ andl(ecx, Operand(dat, alt, NoScale, 4));
    __ movl(Operand(frm, StackOffset(slot) + 4), ecx);

    __ movl(pri, Operand(frmAddr()));
    __ addl(pri, StackOffset(slot));
    return true;
}

bool
Compiler::visitXOR_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));
    __ movl(ecx, Operand(dat, pri, NoScale, 0));
    __ xorl(ecx, Operand(dat, alt, NoScale, 0));
    __ movl(Operand(frm, StackOffset(slot)), ecx);
    __ movl(ecx, Operand(dat, pri, NoScale, 4));
    __ xorl(ecx, Operand(dat, alt, NoScale, 4));
    __ movl(Operand(frm, StackOffset(slot) + 4), ecx);

    __ movl(pri, Operand(frmAddr()));
    __ addl(pri, StackOffset(slot));
    return true;
}


bool
Compiler::visitTEST_F32() {
    __ movd(xmm0, pri);
    __ xorps(xmm1, xmm1);
    __ ucomiss(xmm0, xmm1);

    // NaN sets ZF, and so does a successful comparison to 0.0, so we only need
    // a ZF check.
    __ set(not_zero, r8_al);
    return true;
}

bool
Compiler::visitNEG_F32() {
    __ movl(ecx, 0x80000000);
    __ xorl(pri, ecx);
    return true;
}

bool
Compiler::visitMUL_F32() {
    __ movd(xmm0, pri);
    __ movd(xmm1, alt);
    __ mulss(xmm0, xmm1);
    __ movd(pri, xmm0);
    return true;
}

bool
Compiler::visitDIV_ALT_F32() {
    __ movd(xmm0, alt);
    __ movd(xmm1, pri);
    __ divss(xmm0, xmm1);
    __ movd(pri, xmm0);
    return true;
}

bool
Compiler::visitADD_F32() {
    __ movd(xmm0, pri);
    __ movd(xmm1, alt);
    __ addss(xmm0, xmm1);
    __ movd(pri, xmm0);
    return true;
}

bool
Compiler::visitSUB_ALT_F32() {
    __ movd(xmm0, alt);
    __ movd(xmm1, pri);
    __ subss(xmm0, xmm1);
    __ movd(pri, xmm0);
    return true;
}

bool
Compiler::visitCompareOpF32(CompareOp op) {
    __ movd(xmm0, pri);
    __ movd(xmm1, alt);

    auto cc = ToFloatConditionCode(op);
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

        __ ucomiss(xmm0, xmm1);
    } else {
        __ ucomiss(xmm1, xmm0);
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
    return true;
}

bool
Compiler::visitCVT_F32() {
    __ cvtsi2ss(xmm0, pri);
    __ movd(pri, xmm0);
    return true;
}

bool
Compiler::visitMOD_ALT_F32() {
    static const size_t kStackNeeded = 2 * sizeof(void*);
    static const size_t kStackReserve = ke::Align(kStackNeeded, 16);
    __ subl(esp, kStackReserve);
    __ movl(Operand(esp, 1 * sizeof(void*)), pri);
    __ movl(Operand(esp, 0 * sizeof(void*)), alt);
    __ callWithABI(ExternalAddress((void*)::fmodf));
    __ fstp32(Operand(esp, 0));
    __ movl(pri, Operand(esp, 0));
    __ addl(esp, kStackReserve);
    return true;
}

bool
Compiler::visitCompareOp64(CompareOp op) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ push(ebx);

    switch (op) {
        case CompareOp::Eq:
        case CompareOp::Neq:
            __ lea(edx, Operand(dat, alt, NoScale, 0));
            __ lea(ecx, Operand(dat, pri, NoScale, 0));
            __ movl(eax, Operand(edx, 0));
            __ movl(edx, Operand(edx, 4));
            __ xorl(eax, Operand(ecx, 0));
            __ xorl(edx, Operand(ecx, 4));
            __ orl(eax, edx);
            __ set(equal, eax);
            __ set(op == CompareOp::Eq ? equal : not_equal, eax);
            break;
        case CompareOp::Sless:
        case CompareOp::Sgrtr:
            if (op == CompareOp::Sless) {
                __ lea(ecx, Operand(dat, pri, NoScale, 0));
                __ lea(edx, Operand(dat, alt, NoScale, 0));
            } else {
                __ lea(ecx, Operand(dat, alt, NoScale, 0));
                __ lea(edx, Operand(dat, pri, NoScale, 0));
            }
            __ movl(eax, Operand(ecx, 4));
            __ movl(ebx, Operand(edx, 0));
            __ cmpl(Operand(ecx, 0), ebx);
            __ sbbl(eax, Operand(edx, 4));
            __ set(less, eax);
            break;
        case CompareOp::Sleq:
        case CompareOp::Sgeq:
            if (op == CompareOp::Sleq) {
                __ lea(ecx, Operand(dat, alt, NoScale, 0));
                __ lea(edx, Operand(dat, pri, NoScale, 0));
            } else {
                __ lea(ecx, Operand(dat, pri, NoScale, 0));
                __ lea(edx, Operand(dat, alt, NoScale, 0));
            }
            __ movl(ebx, Operand(edx, 0));
            __ movl(eax, Operand(ecx, 4));
            __ cmpl(Operand(ecx, 0), ebx);
            __ sbbl(eax, Operand(edx, 4));
            __ set(greater_equal, eax);
            break;
        default:
            assert(false);
    }

    __ pop(ebx);
    __ movzxb(eax, eax);
    return true;
}

bool Compiler::visitSTOR_S_PRI_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));

    __ movl(tmp, Operand(dat, pri, NoScale, 0));
    __ movl(Operand(frm, StackOffset(slot)), tmp);
    __ movl(tmp, Operand(dat, pri, NoScale, 4));
    __ movl(Operand(frm, StackOffset(slot) + 4), tmp);
    return true;
}

bool Compiler::visitSTOR_S_C(cell_t slot, cell_t value) {
    __ movl(Operand(frm, StackOffset(slot)), value);
    return true;
}

void
Compiler::jumpOnError(ConditionCode cc, int err) {
    // Note: we accept 0 for err. In this case we expect the error to be in eax.
    ErrorThunk thunk(op_cip_, err);

    __ j(cc, &thunk.label);
    error_thunks_.emplace_back(std::move(thunk));
}

void
Compiler::emitOutOfBoundsError(OutOfBoundsError* path) {
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
Compiler::emitErrorHandlers() {
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
Compiler::emitThrowPath(int err) {
    __ movl(eax, err);
    __ jmp(&report_error_);
}

void
Compiler::emitDebugBreakHandler() {
    // Common path for invoking debugger.
    __ bind(&debug_break_);

    // Get and store the current stack pointer.
    __ movl(tmp, stk);
    __ subl(tmp, dat);
    __ movl(Operand(spAddr()), tmp);

    // Enter the exit frame. This aligns the stack.
    __ enterExitFrame(ExitFrameType::Helper, 0);

    // Allocate enough memory to keep the stack aligned.
    static const size_t kStackNeeded = 2 * sizeof(void*);
    static const size_t kStackReserve = ke::Align(kStackNeeded, 16);
    __ subl(esp, kStackReserve);

    // Get the context pointer and call the debugging break handler.
    __ movl(Operand(esp, 1 * sizeof(void*)), 0); // IErrorReport*
    __ movl(Operand(esp, 0 * sizeof(void*)), intptr_t(rt_->GetBaseContext()));
    __ call(ExternalAddress((void*)InvokeDebugger));
    __ leaveExitFrame();
    __ testl(eax, eax);
    jumpOnError(not_zero);
    __ ret();
}

void CompilerBase::PatchCallThunk(uint8_t* pc, void* target) {
    *(intptr_t*)(pc - 4) = intptr_t(target) - intptr_t(pc);
}

bool CompilerBase::IsSupported() {
    return FeaturesX86::Get().fpu && FeaturesX86::Get().sse && FeaturesX86::Get().sse2;
}

bool CompilerBase::SupportsPlugin(Runtime* cx) {
    return true;
}

} // namespace sp::v2
