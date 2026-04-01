// vim: set ts=8 sts=4 sw=4 tw=99 et:
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
#include "jit_x64.h"

#include <math.h>

#define __ masm.

#include "code-stubs.h"
#include "debugging.h"
#include "environment.h"
#include "features-x64.h"
#include "method-info.h"
#include "runtime-helpers.h"

#define __ masm.

namespace sp {

Compiler::Compiler(PluginRuntime* rt, MethodInfo* method)
 : CompilerBase(rt, method)
{}

Compiler::~Compiler()
{}

void Compiler::emitPrologue() {
    size_t frame_items = __ enterFrame(JitFrameType::Scripted, pcode_start_) + 1;
    (void)frame_items;

    assert(frame_items == 3);

    // The incoming call misaligned the stack, and pushing our frame (two words)
    // did not change the alignment. Fix it now.
    __ subq(rsp, 8);

    // Push the old frame onto the stack.
    __ subq(stk, 8);
    __ movl(tmp, frmAddr());
    __ movl(Operand(stk, 4), tmp);
    __ movl(tmp, hpAddr());
    __ movl(Operand(stk, 0), tmp);

    // Get and store the new frame.
    __ movq(tmp, stk);
    __ movq(frm, stk);
    __ subq(tmp, dat);
    __ movl(Operand(frmAddr()), tmp);

    int32_t max_stack = method_info_->max_stack();
    assert(max_stack >= 0);

    if (max_stack) {
        __ movl(rax, Operand(hpAddr()));
        __ lea(rax, Operand(dat, rax, NoScale, STACK_MARGIN));
        __ lea(rcx, Operand(stk, -max_stack));
        __ cmpq(rcx, rax);
        jumpOnError(below, SP_ERROR_STACKLOW);
    }

    if (cell_t stack_needed = method_info_->StackSizeForLocalSlots())
        __ addq(stk, stack_needed);
}

void Compiler::emitThrowPath(int err) {
    __ movl(rax, err);
    __ jmp(&report_error_);
}

void Compiler::emitErrorHandlers() {
    Label return_to_invoke;

    if (report_error_.used()) {
        __ bind(&report_error_);

        // Create the exit frame. We always get here through a call from the opcode
        // (and always via an out-of-line thunk).
        size_t frame_items = __ enterExitFrame(ExitFrameType::Helper, 0) + 1;

        // Align the stack and call.
        __ movl(ArgReg0, rax);
        __ callWithABI(frame_items, ExternalAddress((void*)InvokeReportError));
        __ leaveExitFrame();

        // Return address is still on the stack, so jmp instead of call.
        __ jmp(&return_to_invoke);
    }

    // The timeout uses a special stub.
    if (throw_timeout_.used()) {
        __ bind(&throw_timeout_);

        // Create the exit frame.
        size_t frame_items = __ enterExitFrame(ExitFrameType::Helper, 0) + 1;

        // Since the return stub wipes out the stack, we don't need to addl after
        // the call.
        __ callWithABI(frame_items, ExternalAddress((void*)InvokeReportTimeout));
        __ leaveExitFrame();
        __ jmp(&return_to_invoke);
    }

    // We get here if we know an exception is already pending.
    if (return_reported_error_.used()) {
        __ bind(&return_reported_error_);
        __ call(&return_to_invoke);
    }

    if (return_to_invoke.used()) {
        __ bind(&return_to_invoke);

        size_t frame_items = __ enterExitFrame(ExitFrameType::Helper, 0) + 1;

        // We cannot jump to the return stub just yet. We could be multiple frames
        // deep, and our |ebp| does not match the initial frame. Find and restore
        // it now.
        __ callWithABI(frame_items, ExternalAddress((void*)find_entry_fp));
        __ leaveExitFrame();

        __ movq(rbp, rax);
        __ jmp(AddressValue(env_->stubs()->ReturnStub()));
    }
}

void Compiler::emitOutOfBoundsError(OutOfBoundsError* path) {
    RipCodeLabel return_address;
    size_t frame_items = __ pushInlineExitFrame(ExitFrameType::Helper, 0, &return_address);
    __ movl(ArgReg1, path->bounds);
    __ movl(ArgReg0, rax);
    size_t alignment = __ callWithABI(frame_items, ExternalAddress((void*)ReportOutOfBoundsError));
    __ bind(&return_address);
    emitCipMapping(path->cip);
    __ popInlineExitFrame(alignment);
    __ jmp(&return_reported_error_);
}

bool Compiler::beforeVisitOp(OPCODE op) {
#ifndef NDEBUG
    __ movq(rcx, op);
#endif
    return true;
}

void Compiler::emitDebugBreakHandler() {
    // Common path for invoking debugger.
    __ bind(&debug_break_);

    // Get and store the current stack pointer.
    __ movq(tmp, stk);
    __ subq(tmp, dat);
    __ movl(Operand(spAddr()), tmp);

    // Enter the exit frame. This aligns the stack.
    size_t frame_items = __ enterExitFrame(ExitFrameType::Helper, 0) + 1;

    // Get the context pointer and call the debugging break handler.
    __ xorq(ArgReg1, ArgReg1);
    __ movq(ArgReg0, context_reg);
    __ callWithABI(frame_items, ExternalAddress((void*)InvokeDebugger));
    __ leaveExitFrame();
    __ testl(rax, rax);
    jumpOnError(not_zero);
    __ ret();
}

bool CompilerBase::IsSupported() {
    const auto& features = FeaturesX64::Get();
    return features.sse4_1;
}

bool CompilerBase::SupportsPlugin(PluginContext* cx) {
    const auto& code = cx->runtime()->code();
    if (code.version < SmxConsts::CODE_VERSION_FEATURE_MASK)
        return false;

    uint32_t required_features =
        SmxConsts::kCodeFeatureDirectArrays |
        SmxConsts::kCodeFeatureHeapScopes |
        SmxConsts::kCodeFeatureNullFunctions |
        SmxConsts::kCodeFeatureTypedOps;
    return (code.features & required_features) == required_features;
}

void CompilerBase::PatchCallThunk(uint8_t* pc, void* target) {
    Assembler::PatchCallThunk(pc, reinterpret_cast<uintptr_t>(target));
}

bool Compiler::visitBREAK() {
    if (!Environment::get()->IsDebugBreakEnabled())
        return true;

    __ call(&debug_break_);
    emitCipMapping(op_cip_);
    return true;
}

bool Compiler::visitLOAD(PawnReg dest, cell_t srcaddr) {
    Register reg = (dest == PawnReg::Pri) ? pri : alt;
    __ movl(reg, Operand(dat, srcaddr));
    return true;
}

bool Compiler::visitLOAD_S(PawnReg dest, cell_t srcoffs) {
    Register reg = (dest == PawnReg::Pri) ? pri : alt;
    __ movl(reg, Operand(frm, StackOffset(srcoffs)));
    return true;
}

bool Compiler::visitLREF_S(PawnReg dest, cell_t srcoffs) {
    Register reg = (dest == PawnReg::Pri) ? pri : alt;
    __ movl(reg, Operand(frm, StackOffset(srcoffs)));
    __ movl(reg, Operand(dat, reg, NoScale));
    return true;
}

bool Compiler::visitLOAD_I() {
    emitCheckAddress(pri);
    __ movl(pri, Operand(dat, pri, NoScale));
    return true;
}

bool Compiler::visitLODB_I(cell_t width) {
    emitCheckAddress(pri);
    __ movl(pri, Operand(dat, pri, NoScale));
    if (width == 1)
        __ andl(pri, 0xff);
    else if (width == 2)
        __ andl(pri, 0xffff);
    return true;
}

bool Compiler::visitCONST(PawnReg dest, cell_t imm) {
    Register reg = (dest == PawnReg::Pri) ? pri : alt;
    __ movl(reg, imm);
    return true;
}

bool Compiler::visitADDR(PawnReg dest, cell_t offset) {
    Register reg = (dest == PawnReg::Pri) ? pri : alt;
    __ movl(reg, frmAddr());
    __ addl(reg, StackOffset(offset));
    return true;
}

bool Compiler::visitSTOR(cell_t offset, PawnReg src) {
    Register reg = (src == PawnReg::Pri) ? pri : alt;
    __ movl(Operand(dat, offset), reg);
    return true;
}

bool Compiler::visitSTOR_S(cell_t offset, PawnReg src) {
    Register reg = (src == PawnReg::Pri) ? pri : alt;
    __ movl(Operand(frm, StackOffset(offset)), reg);
    return true;
}

bool Compiler::visitSREF_S(cell_t offset, PawnReg src) {
    Register reg = (src == PawnReg::Pri) ? pri : alt;
    __ movl(tmp, Operand(frm, StackOffset(offset)));
    __ movl(Operand(dat, tmp, NoScale), reg);
    return true;
}

bool Compiler::visitSTOR_I() {
    emitCheckAddress(alt);
    __ movl(Operand(dat, alt, NoScale), pri);
    return true;
}

bool Compiler::visitSTRB_I(cell_t width) {
    emitCheckAddress(alt);
    if (width == 1)
        __ movb(Operand(dat, alt, NoScale), pri);
    else if (width == 2)
        __ movw(Operand(dat, alt, NoScale), pri);
    else if (width == 4)
        __ movl(Operand(dat, alt, NoScale), pri);
    return true;
}

bool Compiler::visitLIDX() {
    assert(false);
    return false;
}

bool Compiler::visitIDXADDR() {
    __ movsxd(pri, pri);
    __ lea(pri, Operand(alt, pri, ScaleFour));
    return true;
}

bool Compiler::visitMOVE(PawnReg reg) {
    if (reg == PawnReg::Pri)
        __ movq(pri, alt);
    else
        __ movq(alt, pri);
    return true;
}

bool Compiler::visitXCHG() {
    __ xchgq(pri, alt);
    return true;
}

bool Compiler::visitPUSH(PawnReg src) {
    Register reg = (src == PawnReg::Pri) ? pri : alt;
    __ movl(Operand(stk, -4), reg);
    __ subq(stk, 4);
    return true;
}

bool Compiler::visitPUSH_C(const cell_t* vals, size_t nvals) {
    for (size_t i = 1; i <= nvals; i++)
        __ movl(Operand(stk, -(4 * int(i))), vals[i - 1]);
    __ subq(stk, 4 * nvals);
    return true;
}

bool Compiler::visitPUSH(const cell_t* offsets, size_t nvals) {
    assert(false);
    return false;
}

bool Compiler::visitPUSH_S(const cell_t* offsets, size_t nvals) {
    for (size_t i = 1; i <= nvals; i++) {
        __ movl(tmp, Operand(frm, StackOffset(offsets[i - 1])));
        __ movl(Operand(stk, -(4 * int(i))), tmp);
    }
    __ subq(stk, 4 * nvals);
    return true;
}

bool Compiler::visitPOP(PawnReg dest) {
    Register reg = (dest == PawnReg::Pri) ? pri : alt;
    __ movl(reg, Operand(stk, 0));
    __ addq(stk, 4);
    return true;
}

bool Compiler::visitSTACK(cell_t amount) {
    __ addq(stk, amount);
    return true;
}

bool Compiler::visitHEAP(cell_t amount) {
    // Note: this must not clobber PRI.
    __ movl(alt, hpAddr());
    __ addl(hpAddr(), amount);

    if (amount < 0) {
        __ cmpl(hpAddr(), context_->DataSize());
        jumpOnError(below, SP_ERROR_HEAPMIN);
    } else {
        __ movl(tmp, hpAddr());
        __ lea(tmp, Operand(dat, tmp, NoScale, STACK_MARGIN));
        __ cmpl(tmp, stk);
        jumpOnError(above, SP_ERROR_HEAPLOW);
    }
    return true;
}

bool Compiler::visitRETN() {
    for (uint32_t i = 0; i < block_->heap_scope_depth(); i++)
        visitHEAP_RESTORE();

    // Restore the old stack and frame pointer.
    __ movq(stk, frm);
    __ movl(frm, Operand(stk, 4)); // get the old frm
    __ movl(tmp, Operand(stk, 0)); // get the old hp
    __ movl(hpAddr(), tmp);
    __ addq(stk, 8);                  // pop stack
    __ movl(frmAddr(), frm); // store back old frm
    __ addq(frm, dat);                // relocate

    // Remove parameters.
    __ movl(tmp, Operand(stk, 0));
    __ lea(stk, Operand(stk, tmp, ScaleFour, 4));

    __ addq(rsp, 8);
    __ leaveFrame();
    __ ret();
    return true;
}

bool Compiler::visitCALL(cell_t offset) {
    RefPtr<MethodInfo> method = rt_->GetMethod(offset);
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

void Compiler::emitCallThunk(CallThunk* thunk) {
    size_t frame_items = __ enterExitFrame(ExitFrameType::Helper, 0);

    // Need one slot on the stack to store the returned code address. Account
    // for the return addressed pushed by the incoming call.
    size_t stack_used = AllocatePreCallStack(sizeof(void*), frame_items + 1);
    __ subq(rsp, stack_used);

    __ movq(ArgReg3, Operand(rsp, stack_used + frame_items * sizeof(void*)));
    __ lea(ArgReg2, Operand(rsp, kShadowStackSize));
    __ movq(ArgReg1, thunk->pcode_offset);
    __ movq(ArgReg0, context_reg);
    __ callWithABI(ExternalAddress((void*)CompileFromThunk));
    __ movq(rdx, Operand(rsp, kShadowStackSize));
    __ addq(rsp, stack_used);
    __ leaveExitFrame();

    __ testl(rax, rax);
    jumpOnError(not_zero);

    __ jmp(rdx);
}

static inline ConditionCode OpToCondition(CompareOp op) {
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

bool Compiler::visitJcmp(CompareOp op, cell_t offset) {
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

bool Compiler::visitSHL() {
    __ movl(rcx, alt);
    __ shll_cl(pri);
    return true;
}

bool Compiler::visitSHR() {
    __ movl(rcx, alt);
    __ shrl_cl(pri);
    return true;
}

bool Compiler::visitSSHR() {
    __ movl(rcx, alt);
    __ sarl_cl(pri);
    return true;
}

bool Compiler::visitSHL_C(PawnReg dest, cell_t amount) {
    Register reg = (dest == PawnReg::Pri) ? pri : alt;
    __ shll(reg, amount);
    return true;
}

bool Compiler::visitSMUL() {
    __ imull(pri, alt);
    return true;
}

bool Compiler::visitSDIV(PawnReg dest) {
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
        __ movl(rdx, dividend);
    else
        __ movl(rax, dividend);
    __ sarl(rdx, 31);
    __ idivl(tmp);
    return true;
}

bool Compiler::visitSDIV_ALT_I32() {
    visitSDIV(PawnReg::Alt);
    return true;
}

bool Compiler::visitSMOD_ALT_I32() {
    visitSDIV(PawnReg::Alt);
    __ movl(rax, rdx);
    return true;
}

bool Compiler::visitADD() {
    __ addl(pri, alt);
    return true;
}

bool Compiler::visitSUB() {
    assert(false);
    return false;
}

bool Compiler::visitSUB_ALT() {
    __ movl(tmp, alt);
    __ subl(tmp, pri);
    __ movl(pri, tmp);
    return true;
}

bool Compiler::visitAND() {
    __ andl(pri, alt);
    return true;
}

bool Compiler::visitOR() {
    __ orl(pri, alt);
    return true;
}

bool Compiler::visitXOR() {
    __ xorl(pri, alt);
    return true;
}

bool Compiler::visitNOT() {
    __ testl(rax, rax);
    __ movl(rax, 0);
    __ set(zero, r8_al);
    return true;
}

bool Compiler::visitNEG() {
    __ negl(rax);
    return true;
}

bool Compiler::visitINVERT() {
    __ notl(pri);
    return true;
}

bool Compiler::visitADD_C(cell_t value) {
    __ addl(pri, value);
    return true;
}

bool Compiler::visitSMUL_C(cell_t value) {
    __ imull(pri, pri, value);
    return true;
}

bool Compiler::visitZERO(PawnReg dest) {
    Register reg = (dest == PawnReg::Pri) ? pri : alt;
    __ xorl(reg, reg);
    return true;
}

bool Compiler::visitZERO(cell_t offset) {
    assert(false);
    return false;
}

bool Compiler::visitZERO_S(cell_t offset) {
    __ movl(Operand(frm, StackOffset(offset)), 0);
    return true;
}

bool Compiler::visitCompareOp(CompareOp op) {
    ConditionCode cc = OpToCondition(op);
    __ cmpl(pri, alt);
    __ movl(pri, 0);
    __ set(cc, r8_al);
    return true;
}

bool Compiler::visitEQ_C(PawnReg src, cell_t value) {
    Register reg = (src == PawnReg::Pri) ? pri : alt;
    __ cmpl(reg, value);
    __ movl(pri, 0);
    __ set(equal, r8_al);
    return true;
}

bool Compiler::visitINC(PawnReg dest) {
    Register reg = (dest == PawnReg::Pri) ? pri : alt;
    __ addl(reg, 1);
    return true;
}

bool Compiler::visitINC(cell_t offset) {
    assert(false);
    return false;
}

bool Compiler::visitINC_S(cell_t offset) {
    assert(false);
    return false;
}

bool Compiler::visitINC_I() {
    assert(false);
    return false;
}

bool Compiler::visitDEC(PawnReg dest) {
    Register reg = (dest == PawnReg::Pri) ? pri : alt;
    __ subl(reg, 1);
    return true;
}

bool Compiler::visitDEC(cell_t offset) {
    assert(false);
    return false;
}

bool Compiler::visitDEC_S(cell_t offset) {
    assert(false);
    return false;
}

bool Compiler::visitDEC_I() {
    assert(false);
    return false;
}

bool Compiler::visitMOVS(uint32_t amount) {
    uint32_t dwords = amount / 4;
    uint32_t bytes = amount % 4;

    __ cld();
    __ lea(rdi, Operand(dat, alt, NoScale));
    __ lea(rsi, Operand(dat, pri, NoScale));
    if (dwords) {
        __ movl(rcx, dwords);
        __ rep_movsd();
    }
    if (bytes) {
        __ movl(rcx, bytes);
        __ rep_movsb();
    }
    return true;
}

bool Compiler::visitFILL(uint32_t amount) {
    // eax/pri is used implicitly.
    unsigned dwords = amount / 4;
    __ lea(rdi, Operand(dat, alt, NoScale));
    __ movl(rcx, dwords);
    __ cld();
    __ rep_stosd();
    return true;
}

bool Compiler::visitBOUNDS(uint32_t limit) {
    OutOfBoundsError error(op_cip_, limit);

    __ cmpl(rax, limit);
    __ j(above, &error.label);

    bounds_errors_.emplace_back(std::move(error));
    return true;
}

bool Compiler::visitSYSREQ_C(uint32_t native_index) {
    assert(false);
    return false;
}

bool Compiler::visitSWAP(PawnReg dest) {
    Register reg = (dest == PawnReg::Pri) ? pri : alt;
    __ movl(tmp, Operand(stk, 0));
    __ movl(Operand(stk, 0), reg);
    __ movl(reg, tmp);
    return true;
}

bool Compiler::visitPUSH_ADR(const cell_t* offsets, size_t nvals) {
    // We temporarily relocate FRM to be a local address instead of an
    // absolute address.
    __ subq(frm, dat);
    for (size_t i = 1; i <= nvals; i++) {
        __ lea(tmp, Operand(frm, StackOffset(offsets[i - 1])));
        __ movl(Operand(stk, -(4 * int(i))), tmp);
    }
    __ subq(stk, 4 * nvals);
    __ addq(frm, dat);
    return true;
}

bool Compiler::visitSYSREQ_N(uint32_t native_index, uint32_t nparams) {
    NativeEntry* native = rt_->NativeAt(native_index);

    // Store the number of parameters on the stack.
    __ movl(Operand(stk, -4), nparams);
    __ subq(stk, 4);
    emitLegacyNativeCall(native_index, native);
    __ addq(stk, (nparams + 1) * sizeof(cell_t));
    return true;
}

void Compiler::emitLegacyNativeCall(uint32_t native_index, NativeEntry* native) {
    RipCodeLabel return_address;
    size_t frame_items = __ pushInlineExitFrame(ExitFrameType::Native, native_index, &return_address);

    // Save registers.
    __ push(rdx);
    frame_items++;

    // Check whether the native is bound.
    bool immutable = native->status == SP_NATIVE_BOUND &&
                     !(native->flags & (SP_NTVFLAG_EPHEMERAL | SP_NTVFLAG_OPTIONAL));
    bool fast_path = immutable && native->legacy_fn;

    // Save the old heap pointer.
    __ movl(rax, hpAddr());
    __ push(rax);
    frame_items++;

    __ movq(ArgReg0, context_reg);
    if (fast_path) {
        __ movq(ArgReg1, stk);
    } else {
        __ movq(ArgReg1, reinterpret_cast<intptr_t>(native));
        __ movq(ArgReg2, stk);
    }

    // Relocate our absolute stk to be dat-relative, and update the context's
    // view.
    __ subq(stk, dat);
    __ movl(spAddr(), stk);

    size_t alignment;
    if (fast_path) {
        // Fast invoke, skip right to the function call.
        alignment = __ callWithABI(frame_items, ExternalAddress((void*)native->legacy_fn));
    } else {
        alignment = __ callWithABI(frame_items, ExternalAddress((void*)NativeInvokeThunk));
    }
    __ bind(&return_address);
    // Map the return address to the cip that initiated this call.
    emitCipMapping(op_cip_);

    if (alignment)
        __ addq(rsp, alignment);

    // Restore the heap pointer.
    __ pop(rdx);
    __ movl(hpAddr(), rdx);

    // Restore ALT.
    __ pop(rdx);

    // Restore SP.
    __ addq(stk, dat);

    // Remove the inline frame, + our four arguments.
    __ popInlineExitFrame();

    // Check for errors. Note we jump directly to the return stub since the
    // error has already been reported.
    ExternalAddress exn_code(Environment::get());
    __ movq(rcx, exn_code);
    __ cmpl(Operand(rcx, Environment::offsetOfExceptionCode()), 0);
    __ j(not_zero, &return_reported_error_);
}

bool Compiler::visitLOAD_BOTH(cell_t offsetForPri, cell_t offsetForAlt) {
    assert(false);
    return false;
}

bool Compiler::visitLOAD_S_BOTH(cell_t offsetForPri, cell_t offsetForAlt) {
    assert(false);
    return false;
}

bool Compiler::visitCONST(cell_t offset, cell_t value) {
    assert(false);
    return false;
}

bool Compiler::visitCONST_S(cell_t offset, cell_t value) {
    assert(false);
    return false;
}

bool Compiler::visitTRACKER_PUSH_C(cell_t amount) {
    assert(false);
    return false;
}

bool Compiler::visitTRACKER_POP_SETHEAP() {
    assert(false);
    return false;
}

static int
InvokeGenerateFullArray(PluginContext* cx, uint32_t argc, cell_t* argv, int autozero) {
    return cx->generateFullArray(argc, argv, autozero);
}

bool Compiler::visitGENARRAY(uint32_t dims, bool autozero) {
    assert(rt_->UsesHeapScopes());

    if (dims == 1) {
        // flat array; we can generate this without indirection tables.
        // Note that we can overwrite ALT because technically STACK should be destroying ALT
        __ movl(alt, Operand(hpAddr()));
        __ movl(tmp, Operand(stk, 0));
        __ cmpl(tmp, 0);
        jumpOnError(less_equal, SP_ERROR_INVALID_ARRAY_SIZE);
        __ movl(Operand(stk, 0), alt); // store base of the array into the stack.
        __ lea(rdi, Operand(dat, alt, NoScale));
        __ lea(alt, Operand(alt, tmp, ScaleFour));
        __ lea(r8, Operand(dat, alt, NoScale));
        __ cmpq(r8, stk);
        jumpOnError(not_below, SP_ERROR_HEAPLOW);
        __ movl(Operand(hpAddr()), alt);

        if (autozero) {
            // Note - tmp is ecx and still intact.
            __ push(rax);
            __ xorl(rax, rax);
            __ cld();
            __ rep_stosd();
            __ pop(rax);
        }
    } else {
        // We need to sync |sp| first.
        __ subq(stk, dat);
        __ movl(Operand(spAddr()), stk);
        __ addq(stk, dat);

        // No inline exit frame needed here, the helper doesn't throw exceptions.
        size_t stack_space = AllocatePreCallStack(sizeof(void*), 0);
        __ subq(rsp, stack_space);
        __ movl(Operand(rsp, kShadowStackSize), pri);

        // int GenerateArray(cx, vars[], uint32_t, cell_t*, int, unsigned*);
        __ movl(ArgReg3, autozero ? 1 : 0);
        __ movq(ArgReg2, stk);
        __ movl(ArgReg1, dims);
        __ movq(ArgReg0, context_reg);
        __ callWithABI(ExternalAddress((void*)InvokeGenerateFullArray));

        // restore pri to tmp
        __ movl(tmp, Operand(rsp, kShadowStackSize));
        __ addq(rsp, stack_space);

        __ testl(rax, rax);
        jumpOnError(not_zero);

        // Move tmp back to pri, remove pushed args.
        __ movl(pri, tmp);
        __ addq(stk, (dims - 1) * 4);
    }
    return true;
}

bool Compiler::visitSTRADJUST_PRI() {
    __ addl(pri, 4);
    __ sarl(pri, 2);
    return true;
}

bool Compiler::visitFABS() {
    __ movl(pri, Operand(stk, 0));
    __ andl(pri, 0x7fffffff);
    __ addq(stk, 4);
    return true;
}

bool Compiler::visitFLOAT() {
    assert(false);
    return false;
}

bool Compiler::visitFLOATADD() {
    assert(false);
    return false;
}

bool Compiler::visitFLOATSUB() {
    assert(false);
    return false;
}

bool Compiler::visitFLOATMUL() {
    assert(false);
    return false;
}

bool Compiler::visitFLOATDIV() {
    assert(false);
    return false;
}

bool Compiler::visitRND_TO_NEAREST() {
    // Docs say that MXCSR must be preserved across function calls, so we
    // assume that we'll always get the defualt round-to-nearest.
    __ cvtss2si(pri, Operand(stk, 0));
    __ addq(stk, 4);
    return true;
}

bool Compiler::visitRND_TO_FLOOR() {
    __ roundss_floor(xmm0, Operand(stk, 0));
    __ cvttss2si(pri, xmm0);
    __ addq(stk, 4);
    return true;
}

bool Compiler::visitRND_TO_CEIL() {
    __ roundss_ceil(xmm0, Operand(stk, 0));
    __ cvttss2si(pri, xmm0);
    __ addq(stk, 4);
    return true;
}

bool Compiler::visitRND_TO_ZERO() {
    __ cvttss2si(pri, Operand(stk, 0));
    __ addq(stk, 4);
    return true;
}

bool Compiler::visitFLOATCMP() {
    assert(false);
    return false;
}

bool Compiler::visitFLOAT_CMP_OP(CompareOp op) {
    assert(false);
    return false;
}

bool Compiler::visitFLOAT_NOT() {
    assert(false);
    return false;
}

bool Compiler::visitHALT(cell_t value) {
    assert(false);
    return false;
}

bool Compiler::visitSWITCH(cell_t defaultOffset, const CaseTableEntry* cases, size_t ncases) {
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
        RipDataLabel table;
        __ lea(rsi, &table);
        __ movsxd(rcx, rcx);
        __ movq(rdi, Operand(rsi, rcx, ScaleEight));
        __ jmp(rdi);

        // We emit absolute addresses in reverse order in the assembler, to
        // avoid rip-relative fixups during linking. This means we have to
        // walk the case statements backwards, to make sure the table can
        // be read in forward order.
        for (size_t i = ncases - 1; i < ncases; i--) {
            Block* target = block_->successors()[i + 1];
            __ emit_absolute_address(target->address_label());
        }
        __ bind(&table);
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

struct InitArrayArgs {
    cell_t dat_addr;
    cell_t iv_size;
    cell_t data_copy_size;
    cell_t data_fill_size;
    cell_t fill_value;
};

int InvokeInitArray(PluginContext* cx, cell_t base_addr, InitArrayArgs* args) {
    return cx->initArray(base_addr, args->dat_addr, args->iv_size, args->data_copy_size,
                         args->data_fill_size, args->fill_value) ? 1 : 0;
}

bool Compiler::visitINITARRAY(PawnReg reg, cell_t addr, cell_t iv_size, cell_t data_copy_size,
                              cell_t data_fill_size, cell_t fill_value) {
    if (!iv_size) {
        // This is a flat array, we can inline something a little faster.
        if (reg == PawnReg::Pri)
            __ lea(rdi, Operand(dat, pri, NoScale));
        else
            __ lea(rdi, Operand(dat, alt, NoScale));
        if (data_copy_size) {
            __ lea(rsi, Operand(dat, addr));
            __ cld();
            __ movl(rcx, data_copy_size);
            __ rep_movsd();
        }
        if (data_fill_size) {
            __ movl(rax, fill_value);
            __ movl(rcx, data_fill_size);
            __ rep_stosd();
        }
    } else {
        // Slow (multi-d) array initialization.
        // We need to sync |sp| first.
        __ movq(rcx, stk);
        __ subq(rcx, dat);
        __ movl(spAddr(), rcx);

        size_t frame_header_items = __ enterExitFrame(ExitFrameType::Helper, 0);

        __ push(alt);
        frame_header_items++;

        // Too many parameters to make this easy on x64, so we use a struct.
        size_t args_size = AllocatePreCallStack(sizeof(InitArrayArgs), frame_header_items);
        __ subq(rsp, args_size);

        // Make sure pri/alt get used first since rdx is used for argument passing.
        __ movq(ArgReg1, (reg == PawnReg::Pri) ? pri : alt);
        __ lea(ArgReg2, Operand(rsp, kShadowStackSize));
        __ movl(Operand(ArgReg2, offsetof(InitArrayArgs, fill_value)), fill_value);
        __ movl(Operand(ArgReg2, offsetof(InitArrayArgs, data_fill_size)), data_fill_size);
        __ movl(Operand(ArgReg2, offsetof(InitArrayArgs, data_copy_size)), data_copy_size);
        __ movl(Operand(ArgReg2, offsetof(InitArrayArgs, iv_size)), iv_size);
        __ movl(Operand(ArgReg2, offsetof(InitArrayArgs, dat_addr)), addr);
        __ movq(ArgReg0, context_reg);
        __ callWithABI(ExternalAddress((void*)InvokeInitArray));
        __ addq(rsp, args_size);

        __ pop(alt);

        __ leaveExitFrame();

        __ testl(rax, rax);
        __ j(zero, &return_reported_error_);
    }
    return true;
}

bool Compiler::visitHEAP_SAVE() {
    // Allocate one cell on the heap.
    visitHEAP(sizeof(cell_t));
    // Get the addres of the old heap scope in pri.
    __ movl(pri, hpScopeAddr());
    // Store the old heap scope address into the new heap scope.
    __ movl(Operand(dat, alt, NoScale), pri);
    // Update the context's current heap scope.
    __ movl(hpScopeAddr(), alt);
    return true;
}

bool Compiler::visitHEAP_RESTORE() {
    // Get the current heap scope address.
    __ movl(rcx, hpScopeAddr());
    // Get the previous heap scope address.
    __ movl(alt, Operand(dat, rcx, NoScale));
    // Update the heap pointer.
    __ movl(hpAddr(), rcx);
    // Update the heap scope.
    __ movl(hpScopeAddr(), alt);
    return true;
}

bool Compiler::visitPUSH_I_I64() {
    emitCheckAddress(pri, sizeof(int64_t));
    __ movq(tmp, Operand(dat, pri, NoScale, 0));
    __ movq(Operand(stk, -8), tmp);
    __ subq(stk, 8);
    return true;
}

bool Compiler::visitMOVE_I64() {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ movq(rcx, Operand(dat, pri, NoScale, 0));
    __ movq(Operand(dat, alt, NoScale, 0), rcx);
    return true;
}

bool Compiler::visitCVT_I64(cell_t slot) {
    __ movsxd(tmp, pri);
    __ lea(pri, Operand(frm, StackOffset(slot)));
    __ movq(Operand(pri, 0), tmp);
    __ subq(pri, dat);
    return true;
}

bool Compiler::visitTRUNCATE_I64() {
    emitCheckAddress(pri, sizeof(int64_t));

    __ movl(pri, Operand(dat, pri, NoScale, 0));
    return true;
}

bool Compiler::visitTEST_I64() {
    emitCheckAddress(pri, sizeof(int64_t));

    __ movq(rcx, Operand(dat, pri, NoScale, 0));
    __ testq(rcx, rcx);
    __ set(not_zero, r8_al);
    __ movzxb(pri, pri);
    return true;
}

bool Compiler::visitINVERT_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));

    __ movq(rcx, Operand(dat, pri, NoScale, 0));
    __ notq(rcx);
    __ lea(rax, Operand(frm, StackOffset(slot)));
    __ movq(Operand(rax, 0), rcx);

    __ subq(rax, dat);
    return true;
}

bool Compiler::visitNEG_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));

    __ movq(rcx, Operand(dat, pri, NoScale, 0));
    __ negq(rcx);
    __ lea(rax, Operand(frm, StackOffset(slot)));
    __ movq(Operand(rax, 0), rcx);

    __ subq(rax, dat);
    return true;
}

bool Compiler::visitSMUL_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ movq(rdi, Operand(dat, pri, NoScale, 0));
    __ movq(rsi, Operand(dat, alt, NoScale, 0));
    __ lea(rax, Operand(frm, StackOffset(slot)));
    __ imulq(rdi, rsi);
    __ movq(Operand(rax, 0), rdi);

    __ subq(rax, dat);
    return true;
}

bool Compiler::visitSDIV_ALT_I64(cell_t pri_slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ movq(pri, Operand(dat, pri, NoScale, 0));
    __ movq(alt, Operand(dat, alt, NoScale, 0));

    const Register dividend = alt;
    const Register divisor = pri;

    // Guard against divide-by-zero.
    __ testq(divisor, divisor);
    jumpOnError(zero, SP_ERROR_DIVIDE_BY_ZERO);

    // A more subtle case; -INT_MIN / -1 yields an overflow exception.
    Label ok;
    __ cmpq(divisor, -1);
    __ j(not_equal, &ok);
    __ movq(rcx, std::numeric_limits<int64_t>::min());
    __ cmpq(dividend, rcx);
    jumpOnError(equal, SP_ERROR_INTEGER_OVERFLOW);
    __ bind(&ok);

    // Now we can actually perform the divide.
    __ movq(tmp, divisor);
    __ movq(rax, dividend);
    __ sarq(rdx, 63);
    __ idivq(tmp);

    __ movq(rcx, rax);
    __ lea(rax, Operand(frm, StackOffset(pri_slot)));
    __ movq(Operand(rax, 0), rcx);
    __ subq(rax, dat);
    return true;
}

bool Compiler::visitSMOD_ALT_I64(cell_t pri_slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ movq(pri, Operand(dat, pri, NoScale, 0));
    __ movq(alt, Operand(dat, alt, NoScale, 0));

    const Register dividend = alt;
    const Register divisor = pri;

    // Guard against divide-by-zero.
    __ testq(divisor, divisor);
    jumpOnError(zero, SP_ERROR_DIVIDE_BY_ZERO);

    // A more subtle case; -INT_MIN / -1 yields an overflow exception.
    Label ok;
    __ cmpq(divisor, -1);
    __ j(not_equal, &ok);
    __ movq(rcx, std::numeric_limits<int64_t>::min());
    __ cmpq(dividend, rcx);
    jumpOnError(equal, SP_ERROR_INTEGER_OVERFLOW);
    __ bind(&ok);

    // Now we can actually perform the divide.
    __ movq(tmp, divisor);
    __ movq(rax, dividend);
    __ sarq(rdx, 63);
    __ idivq(tmp);

    __ lea(rax, Operand(frm, StackOffset(pri_slot)));
    __ movq(Operand(rax, 0), rdx);
    __ subq(rax, dat);
    return true;
}

bool Compiler::visitADD_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ movq(rdi, Operand(dat, pri, NoScale, 0));
    __ movq(rsi, Operand(dat, alt, NoScale, 0));
    __ lea(rax, Operand(frm, StackOffset(slot)));
    __ addq(rdi, rsi);

    __ movq(Operand(rax, 0), rdi);
    __ subq(rax, dat);
    return true;
}

bool Compiler::visitSUB_ALT_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ movq(rdi, Operand(dat, pri, NoScale, 0));
    __ movq(rsi, Operand(dat, alt, NoScale, 0));
    __ lea(rax, Operand(frm, StackOffset(slot)));
    __ subq(rsi, rdi);
    __ movq(Operand(rax, 0), rsi);

    __ subq(rax, dat);
    return true;
}

bool Compiler::visitSHL_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ movq(rdi, Operand(dat, pri, NoScale, 0));
    __ movq(rcx, Operand(dat, alt, NoScale, 0));
    __ lea(rax, Operand(frm, StackOffset(slot)));
    __ shlq_cl(rdi);
    __ movq(Operand(rax, 0), rdi);

    __ subq(rax, dat);
    return true;
}

bool Compiler::visitSSHR_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ movq(rdi, Operand(dat, pri, NoScale, 0));
    __ movq(rcx, Operand(dat, alt, NoScale, 0));
    __ lea(rax, Operand(frm, StackOffset(slot)));
    __ sarq_cl(rdi);
    __ movq(Operand(rax, 0), rdi);

    __ subq(rax, dat);
    return true;
}

bool Compiler::visitSHR_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ movq(rdi, Operand(dat, pri, NoScale, 0));
    __ movq(rcx, Operand(dat, alt, NoScale, 0));
    __ lea(rax, Operand(frm, StackOffset(slot)));
    __ shrq_cl(rdi);
    __ movq(Operand(rax, 0), rdi);

    __ subq(rax, dat);
    return true;
}

bool Compiler::visitOR_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ movq(rdi, Operand(dat, pri, NoScale, 0));
    __ movq(rsi, Operand(dat, alt, NoScale, 0));
    __ lea(rax, Operand(frm, StackOffset(slot)));
    __ orq(rdi, rsi);
    __ movq(Operand(rax, 0), rdi);

    __ subq(rax, dat);
    return true;
}

bool Compiler::visitAND_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ movq(rdi, Operand(dat, pri, NoScale, 0));
    __ movq(rsi, Operand(dat, alt, NoScale, 0));
    __ lea(rax, Operand(frm, StackOffset(slot)));
    __ andq(rdi, rsi);
    __ movq(Operand(rax, 0), rdi);

    __ subq(rax, dat);
    return true;
}

bool Compiler::visitXOR_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ movq(rdi, Operand(dat, pri, NoScale, 0));
    __ movq(rsi, Operand(dat, alt, NoScale, 0));
    __ lea(rax, Operand(frm, StackOffset(slot)));
    __ xorq(rdi, rsi);
    __ movq(Operand(rax, 0), rdi);

    __ subq(rax, dat);
    return true;
}

bool Compiler::visitSTOR_S_C_I64(cell_t slot, cell_t cell0, cell_t cell1) {
    Int64CellUnion u(cell0, cell1);
    __ movq(rcx, u.i64);
    __ movq(Operand(frm, StackOffset(slot)), rcx);
    return true;
}

bool Compiler::visitCompareOp64(CompareOp op) {
    emitCheckAddress(pri, sizeof(int64_t));
    emitCheckAddress(alt, sizeof(int64_t));

    __ movq(rdi, Operand(dat, pri, NoScale, 0));
    __ movq(rsi, Operand(dat, alt, NoScale, 0));

    ConditionCode cc = OpToCondition(op);
    __ cmpq(rdi, rsi);
    __ set(cc, r8_al);
    __ movzxb(pri, pri);
    return true;
}

bool Compiler::visitTEST_F32() {
    __ movd(xmm0, pri);
    __ xorps(xmm1, xmm1);
    __ ucomiss(xmm0, xmm1);

    // NaN sets ZF, and so does a successful comparison to 0.0, so we only need
    // a ZF check.
    __ set(not_zero, r8_al);
    return true;
}

bool Compiler::visitNEG_F32() {
    __ movl(rcx, 0x80000000);
    __ xorl(pri, rcx);
    return true;
}

bool Compiler::visitMUL_F32() {
    __ movd(xmm0, pri);
    __ movd(xmm1, alt);
    __ mulss(xmm0, xmm1);
    __ movd(pri, xmm0);
    return true;
}

bool Compiler::visitDIV_ALT_F32() {
    __ movd(xmm0, alt);
    __ movd(xmm1, pri);
    __ divss(xmm0, xmm1);
    __ movd(pri, xmm0);
    return true;
}

bool Compiler::visitADD_F32() {
    __ movd(xmm0, pri);
    __ movd(xmm1, alt);
    __ addss(xmm0, xmm1);
    __ movd(pri, xmm0);
    return true;
}

bool Compiler::visitSUB_ALT_F32() {
    __ movd(xmm0, alt);
    __ movd(xmm1, pri);
    __ subss(xmm0, xmm1);
    __ movd(pri, xmm0);
    return true;
}

ConditionCode
ToFloatConditionCode(CompareOp op) {
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

bool Compiler::visitCompareOpF32(CompareOp op) {
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
        __ movl(rax, (cc == equal) ? 0 : 1);
        __ j(parity, &done);
        __ set(cc, r8_al);
        __ bind(&done);
    } else {
        __ movl(rax, 0);
        __ set(cc, r8_al);
    }
    return true;
}

bool Compiler::visitCVT_F32() {
    __ cvtsi2ss(xmm0, pri);
    __ movd(pri, xmm0);
    return true;
}

bool Compiler::visitMOD_ALT_F32() {
    __ movd(xmm1, pri);
    __ movd(xmm0, alt);
    if (size_t alignment = __ callWithABI(0, ExternalAddress((void*)::fmodf)))
        __ addq(rsp, alignment);
    __ movd(pri, xmm0);
    return true;
}

bool Compiler::visitSTOR_S_PRI_I64(cell_t slot) {
    emitCheckAddress(pri, sizeof(int64_t));

    __ movq(tmp, Operand(dat, pri, NoScale, 0));
    __ movq(Operand(frm, StackOffset(slot)), tmp);
    return true;
}

bool Compiler::visitZERO_S_I64(cell_t offset) {
    __ xorq(tmp, tmp);
    __ movq(Operand(frm, StackOffset(offset)), tmp);
    return true;
}

bool Compiler::visitSTOR_S_C(cell_t slot, cell_t value) {
    __ movl(Operand(frm, StackOffset(slot)), value);
    return true;
}

void Compiler::emitCheckAddress(Register reg, size_t read_size) {
    // Check if we're in memory bounds.
    __ cmpl(reg, context_->HeapSize() - read_size + 1);
    jumpOnError(above_equal, SP_ERROR_MEMACCESS);

    // Check if we're in the invalid region between hp and sp.
    Label done;
    __ cmpl(reg, hpAddr());
    __ j(below, &done);
    __ lea(tmp, Operand(dat, reg, NoScale));
    __ cmpq(tmp, stk);
    jumpOnError(below, SP_ERROR_MEMACCESS);
    __ bind(&done);
}

void Compiler::jumpOnError(ConditionCode cc, int err) {
    ErrorThunk thunk(op_cip_, err);

    __ j(cc, &thunk.label);
    error_thunks_.emplace_back(std::move(thunk));
}

} // namespace sp
