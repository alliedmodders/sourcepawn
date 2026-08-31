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

#include <assert.h>
#include <math.h>

#include "code-stubs.h"
#include "compiled-function.h"
#include "debugging.h"
#include "environment.h"
#include "v2/lowering/ll-op.h"
#include "v2/lowering/llcode.h"
#include "v2/method-info.h"
#include "v2/runtime-helpers.h"
#include "v2/x64/constants-x64.h"
#include "x64/features-x64.h"

#define __ masm.

namespace sp::v2 {

Compiler::Compiler(Runtime* rt, MethodInfo* method)
 : CompilerBase(rt, method) {
}

Compiler::~Compiler() {
}

bool CompilerBase::IsSupported() {
    const auto& features = FeaturesX64::Get();
    return features.sse4_1;
}

bool CompilerBase::SupportsPlugin(Runtime* cx) {
    return true;
}

// Every JIT function gets 64 bytes of stack (eight 8-byte locals). Since x64
// passes arguments via registers we don't need as much scratch space as we do
// on x86. This is enough for shadow stack space on win64 plus local scratch
// storage.
static constexpr int kNativeStackAllowance = 8 * sizeof(intptr_t);

// Handle<> storage is placed at the top of the pre-allocated stack area.
static constexpr int kHandleOffset = -24;

// The caller passes the callee SpFunction on the native stack just above the
// return address.
static constexpr int kCalleeSlotOffset = 16;

// Windows only has four argument registers. When we need more, we have our
// own internal calling convention for helpers. This means less #ifs in our
// code.
#if defined _WIN64
static constexpr Register HelperArgReg4 = r10;
#else
static constexpr Register HelperArgReg4 = ArgReg4;
#endif

void Compiler::EmitPrologue(const FrameInfo& frame) {
    __ enterFrame(JitFrameType::Scripted, method_info_->frame_id());

    // The first push aligns the stack to 16-bytes.
    __ push(frm);
    __ subq(rsp, kNativeStackAllowance);
    __ assertStackAligned();

    __ lea(frm, Operand(dat_reg, stk, NoScale));

    if (frame.frame_size) {
        __ lea(rax, Operand(frm, frame.frame_size));
        __ subq(rax, dat_reg);

        __ movl(rcx, Operand(env_reg, Environment::offsetOfSpTop()));
        __ cmpl(rax, rcx);
        JumpOnError(above, SP_ERROR_STACKLOW);
    }

    if (frame.callee_regs > 0) {
        __ xorq(rax, rax);
        __ movq(rcx, frame.callee_regs);
        __ lea(rdi, Operand(frm, frame.num_params * sizeof(cell_t)));
        __ rep_stosd();
    }

    // Set stk = frm + num_regs * 4.
    __ addl(stk, int32_t(frame.num_regs * sizeof(cell_t)));
    __ movl(Operand(env_reg, Environment::offsetOfSp()), stk);
}

Operand Compiler::StkRelAddr(uint32_t reg) {
    return Operand(stk, -int32_t((ll_->num_regs() - reg) * sizeof(cell_t)));
}

void CompilerBase::PatchCallThunk(uint8_t* pc, void* target) {
    Assembler::PatchCallThunk(pc, reinterpret_cast<uintptr_t>(target));
}

void Compiler::EmitLoadConst(uint16_t reg, cell_t val) {
    __ movl(RegAddr(reg), val);
}

void Compiler::EmitLoadConst64(uint16_t reg, int64_t val) {
    __ movq(rax, val);
    __ movq(RegAddr(reg), rax);
}

void Compiler::EmitAddr(uint16_t src_reg, uint16_t dest_reg) {
    __ lea(rax, StkRelAddr(src_reg));
    __ movl(RegAddr(dest_reg), rax);
}

void Compiler::EmitRetn(LLOp op, std::optional<uint16_t> reg) {
    if (reg)
        __ movl(rax, RegAddr(*reg));
    else
        __ xorl(rax, rax);

    if (op == LL_RETN_A)
        EmitIncRefForArrayEscape(rax, rcx);

    // Restore world's view of stk.
    __ movq(stk, frm);
    __ subq(stk, dat_reg);
    __ movl(Operand(env_reg, Environment::offsetOfSp()), stk);

    // Restore the previous |frm|.
    __ movq(frm, Operand(rbp, -16));

    __ leaveFrame();
    __ ret();
}

void Compiler::EmitNativeCall(uint32_t native_index, uint8_t nargs, uint16_t dest,
                              const std::vector<uint16_t>& args, uint16_t spread_reg)
{
    NativeEntry* native = rt_->NativeAt(native_index);

    RipCodeLabel return_address;
    __ pushInlineExitFrame(ExitFrameType::Native, native_index, &return_address);

    // 8 bytes to re-align after the inline frame.
    // 16 bytes for two locals (for LL_NTVCALL_VA only).
    // shadow stack if needed.
    static constexpr int kStackAlignment = 8 + 16 + kShadowStackSize;
    __ subq(rsp, kStackAlignment);

    // Save the base to the stack.
    __ movl(Operand(rsp, kShadowStackSize + 0), stk);

    // Copy formal arguments.
    __ movl(HeapAddr(stk, 0), nargs);
    for (uint8_t i = 0; i < nargs; i++) {
        uint16_t arg_reg = args[i];
        __ movl(rax, RegAddr(arg_reg));
        __ movl(HeapAddr(stk, (i + 1) * sizeof(cell_t)), rax);
    }

    // Bump up stk.
    __ lea(stk, Operand(stk, (nargs + 1) * sizeof(cell_t)));

    if (spread_reg != LL_INVALID_REG) {
        __ movl(r10, RegAddr(spread_reg));
        __ movl(rax, HeapAddr(r10));

        // Check for stack overflow.
        __ lea(r11, Operand(stk, rax, ScaleFour));
        __ cmpl(r11, Operand(env_reg, Environment::offsetOfSpTop()));
        JumpOnError(above_equal, SP_ERROR_STACKLOW);

        // Bump up the argument count, get the original params vec (stk).
        __ movl(rdi, Operand(rsp, kShadowStackSize + 0));
        __ addl(HeapAddr(rdi, 0), rax);

        // Copy arguments, saving the base ptr first since on both platforms
        // it's one of the registers needed for movsd. Note that the incoming
        // address is a flat array, not a heap array.
        __ lea(rdi, HeapAddr(stk, 0));
        __ lea(rsi, HeapAddr(r10, sizeof(cell_t)));
        __ movl(rcx, rax);
        __ rep_movsd();

        // We calculated the new stk earlier.
        __ movl(stk, r11);
    }

    // Store the new |stk| back.
    __ movl(Operand(env_reg, Environment::offsetOfSp()), stk);

    __ movl(ArgReg2, Operand(rsp, kShadowStackSize + 0));
    __ lea(ArgReg2, HeapAddr(ArgReg2));
    __ movq(ArgReg1, reinterpret_cast<intptr_t>(native));
    __ movq(ArgReg0, context_reg);
    __ callWithABI(ExternalAddress((void*)NativeInvokeThunk));
    __ bind(&return_address);
    EmitCipMapping(op_cip_);

    // Restore the stack.
    __ movl(stk, Operand(rsp, kShadowStackSize + 0));
    __ movl(Operand(env_reg, Environment::offsetOfSp()), stk);

    __ popInlineExitFrame(kStackAlignment);

    // Check for exception.
    __ cmpl(Operand(env_reg, Environment::offsetOfExceptionCode()), 0);
    JumpOnReportedError(not_zero);

    if (dest != LL_INVALID_REG)
        __ movl(RegAddr(dest), rax);
}

void Compiler::EmitScriptedCall(uint32_t method_index, uint8_t nargs, uint16_t dest,
                                const std::vector<uint16_t>& args)
{
    for (uint8_t i = 0; i < nargs; i++) {
        uint16_t arg_reg = args[i];
        __ movl(rax, RegAddr(arg_reg));
        __ movl(Operand(dat_reg, stk, NoScale, i * sizeof(cell_t)), rax);
    }

    RefPtr<MethodInfo> target = rt_->AcquireMethod(method_index);
    assert(target);

    if (!target->jit()) {
        CallThunk thunk(method_index);
        __ call(&thunk.label);
        call_thunks_.emplace_back(std::move(thunk));
    } else {
        __ call(ExternalAddress(target->jit()->GetEntryAddress()));
    }

    EmitCipMapping(op_cip_);

    if (dest != LL_INVALID_REG)
        __ movl(RegAddr(dest), rax);
}

void Compiler::EmitIndirectCall(uint32_t fn_reg, uint8_t nargs, uint16_t dest,
                                const std::vector<uint16_t>& args)
{
    __ movl(rdx, RegAddr(fn_reg));
    __ testl(rdx, rdx);
    JumpOnError(zero, SP_ERROR_NULL_DEREF);

    // This could happen if a runtime has been unloaded, I guess.
    __ movq(rdx, HeapAddr(rdx, offsetof(SpFunction, method)));
    __ testq(rdx, rdx);
    JumpOnError(zero, SP_ERROR_NULL_DEREF);

    for (uint8_t i = 0; i < nargs; i++) {
        uint16_t arg_reg = args[i];
        __ movl(rax, RegAddr(arg_reg));
        __ movl(Operand(dat_reg, stk, NoScale, i * sizeof(cell_t)), rax);
    }

    // Store the callee object on the stack so upvars can be read/stored.
    __ movq(rax, RegAddr(fn_reg));
    __ movq(Operand(rsp, 0), rax);

    auto& thunk = AddIndirectCallThunk(fn_reg);
    __ movq(rdx, Operand(rdx, MethodInfo::offsetOfCompiledFunction()));
    __ testq(rdx, rdx);
    __ j(zero, &thunk.label);

    __ bind(&thunk.return_to);
    __ movq(rax, Operand(rdx, CompiledFunction::offsetOfEntry()));
    __ call(rax);
    EmitCipMapping(op_cip_);

    if (dest != 0xFFFF)
        __ movl(RegAddr(dest), rax);
}

void Compiler::EmitIndirectCallThunk(IndirectCallThunk* thunk) {
    // Grab SpFunction->method->method_index.
    __ movl(rax, RegAddr(thunk->fn_reg));
    __ movq(rax, HeapAddr(rax, offsetof(SpFunction, method)));

    // Note: we reached here via a jmp, so no return address was pushed onto
    // the stack. To account for that, we need to re-align the stack after.
    __ setupExitFrame(ExitFrameType::Helper, 0);
    __ subq(rsp, 8);

    __ movq(ArgReg1, rax);
    __ movq(ArgReg0, intptr_t(context_));
    __ callWithABI(ExternalAddress((void*)IndirectCompileThunk));
    __ leaveExitFrame();

    __ testq(rax, rax);
    JumpOnReportedError(zero);

    __ movq(rdx, rax);
    __ jmp(&thunk->return_to);
}

void Compiler::EmitJump(size_t target_idx) {
    if (IsBlockEmitted(target_idx)) {
        __ jmp32(&block_labels_[target_idx]);
        backward_jumps_.push_back(BackwardJump(masm.pc(), op_cip_));
    } else {
        __ jmp(&block_labels_[target_idx]);
    }
}

void Compiler::EmitJump(LLOp op, uint16_t src_reg, size_t target_idx) {
    __ cmpl(RegAddr(src_reg), 0);

    ConditionCode cc = (op == LL_JZER) ? zero : not_zero;

    if (IsBlockEmitted(target_idx)) {
        __ j32(cc, &block_labels_[target_idx]);
        backward_jumps_.push_back(BackwardJump(masm.pc(), op_cip_));
    } else {
        __ j(cc, &block_labels_[target_idx]);
    }
}

static ConditionCode CmpOpToCondition(LLOp op) {
    switch (op) {
        case LL_JEQ:
        case LL_EQ_I32:
        case LL_EQ_I64:
            return equal;
        case LL_JNEQ:
        case LL_NEQ_I32:
        case LL_NEQ_I64:
            return not_equal;
        case LL_JSLESS:
        case LL_SLESS_I32:
        case LL_SLESS_I64:
            return less;
        case LL_JSLEQ:
        case LL_SLEQ_I32:
        case LL_SLEQ_I64:
            return less_equal;
        case LL_JSGRTR:
        case LL_SGRTR_I32:
        case LL_SGRTR_I64:
            return greater;
        case LL_JSGEQ:
        case LL_SGEQ_I32:
        case LL_SGEQ_I64:
            return greater_equal;
        default:
            assert(false);
            return equal;
    }
}

void Compiler::EmitJumpCmp(LLOp op, uint16_t reg_a, uint16_t reg_b, size_t target_idx) {
    ConditionCode cc = CmpOpToCondition(op);

    __ movl(rax, RegAddr(reg_a));
    __ cmpl(rax, RegAddr(reg_b));

    if (IsBlockEmitted(target_idx)) {
        __ j32(cc, &block_labels_[target_idx]);
        backward_jumps_.push_back(BackwardJump(masm.pc(), op_cip_));
    } else {
        __ j(cc, &block_labels_[target_idx]);
    }
}

void Compiler::EmitCmpI32(LLOp op, uint16_t reg_a, uint16_t reg_b, uint16_t dest) {
    ConditionCode cc = CmpOpToCondition(op);

    __ xorl(rax, rax);
    __ movl(rcx, RegAddr(reg_a));
    __ cmpl(rcx, RegAddr(reg_b));
    __ set(cc, rax);
    __ movl(RegAddr(dest), rax);
}

void Compiler::EmitBasicAlu(LLOp op, uint16_t lhs_reg, uint16_t rhs_reg, uint16_t dest) {
    __ movl(rax, RegAddr(lhs_reg));

    switch (op) {
        case LL_ADD_I32:
            __ addl(rax, RegAddr(rhs_reg));
            break;
        case LL_SMUL_I32:
            __ imull(rax, RegAddr(rhs_reg));
            break;
        case LL_SUB_I32:
            __ subl(rax, RegAddr(rhs_reg));
            break;
        case LL_XOR_I32:
            __ xorl(rax, RegAddr(rhs_reg));
            break;
        case LL_OR_I32:
            __ orl(rax, RegAddr(rhs_reg));
            break;
        case LL_AND_I32:
            __ andl(rax, RegAddr(rhs_reg));
            break;
        case LL_SHL_I32:
            __ movl(rcx, RegAddr(rhs_reg));
            __ shll_cl(rax);
            break;
        case LL_SHR_I32:
            __ movl(rcx, RegAddr(rhs_reg));
            __ shrl_cl(rax);
            break;
        case LL_SSHR_I32:
            __ movl(rcx, RegAddr(rhs_reg));
            __ sarl_cl(rax);
            break;
        default:
            assert(false);
    }

    __ movl(RegAddr(dest), rax);
}

void Compiler::EmitUnaryAlu(LLOp op, uint16_t src_reg, uint16_t dest_reg) {
    __ movl(rax, RegAddr(src_reg));

    switch (op) {
        case LL_INVERT_I32:
            __ notl(rax);
            break;
        case LL_NEG_I32:
            __ negl(rax);
            break;
        case LL_NOT_I32:
            __ testl(rax, rax);
            __ movl(rax, 0);
            __ set(zero, r8_al);
            break;
        case LL_TEST_I32:
            __ testl(rax, rax);
            __ movl(rax, 0);
            __ set(not_zero, r8_al);
            break;
        case LL_CVT_I8:
            __ movsxb(rax, rax);
            break;
        case LL_CVT_I16:
            __ movsxw(rax, rax);
            break;
        default:
            assert(false);
    }

    __ movl(RegAddr(dest_reg), rax);
}

void Compiler::EmitSdivI32(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) {
    __ movl(rax, RegAddr(lhs));
    __ movl(rcx, RegAddr(rhs));

    __ testl(rcx, rcx);
    JumpOnError(zero, SP_ERROR_DIVIDE_BY_ZERO);

    // A more subtle case; -INT_MIN / -1 yields an overflow exception.
    Label ok;
    __ cmpl(rcx, -1);
    __ j(not_equal, &ok);
    __ cmpl(rax, 0x80000000);
    JumpOnError(equal, SP_ERROR_INTEGER_OVERFLOW);
    __ bind(&ok);

    __ movl(rdx, rax);
    __ sarl(rdx, 31);
    __ idivl(rcx);

    if (op == LL_SDIV_I32)
        __ movl(RegAddr(dest), rax);
    else if (op == LL_SMOD_I32)
        __ movl(RegAddr(dest), rdx);
    else
        assert(false);
}

ConditionCode ToFloatConditionCode(LLOp op) {
    switch (op) {
        case LL_GRTR_F32:
            return above;
        case LL_GEQ_F32:
            return above_equal;
        case LL_LEQ_F32:
            return below_equal;
        case LL_LESS_F32:
            return below;
        case LL_EQ_F32:
            return equal;
        case LL_NEQ_F32:
            return not_equal;
        default:
            assert(false);
            return zero;
    }
}

void Compiler::EmitCompareFloat(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) {
    __ movss(xmm0, RegAddr(lhs));
    __ movss(xmm1, RegAddr(rhs));

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
    __ movl(RegAddr(dest), rax);
}

void Compiler::EmitBinaryFloatOp(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) {
    __ movss(xmm0, RegAddr(lhs));
    __ movss(xmm1, RegAddr(rhs));

    switch (op) {
        case LL_ADD_F32:
            __ addss(xmm0, xmm1);
            __ movd(RegAddr(dest), xmm0);
            break;
        case LL_SUB_F32:
            __ subss(xmm0, xmm1);
            __ movd(RegAddr(dest), xmm0);
            break;
        case LL_MUL_F32:
            __ mulss(xmm0, xmm1);
            __ movd(RegAddr(dest), xmm0);
            break;
        case LL_DIV_F32:
            __ divss(xmm0, xmm1);
            __ movd(RegAddr(dest), xmm0);
            break;
        case LL_MOD_F32:
            __ movss(xmm1, RegAddr(rhs));
            __ movss(xmm0, RegAddr(lhs));
            __ callWithABI(ExternalAddress((void*)::fmodf));
            __ movd(RegAddr(dest), xmm0);
            break;
        default:
            assert(false);
            break;
    }
}

void Compiler::EmitUnaryFloatOp(LLOp op, uint16_t src_reg, uint16_t dest_reg) {
    switch (op) {
        case LL_CVT_F32:
            __ cvtsi2ss(xmm0, RegAddr(src_reg));
            __ movd(RegAddr(dest_reg), xmm0);
            break;
        case LL_TEST_F32:
            __ movss(xmm0, RegAddr(src_reg));
            __ xorps(xmm1, xmm1);
            __ ucomiss(xmm0, xmm1);

            // NaN sets ZF, and so does a successful comparison to 0.0, so we only need
            // a ZF check.
            __ set(not_zero, r8_al);
            __ movl(RegAddr(dest_reg), rax);
            break;
        case LL_NEG_F32:
            __ movl(rax, RegAddr(src_reg));
            __ movl(rcx, 0x80000000);
            __ xorl(rax, rcx);
            __ movl(RegAddr(dest_reg), rax);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitMove(LLOp op, uint16_t src_reg, uint16_t dest_reg) {
    switch (op) {
        case LL_MOVE:
            __ movl(rax, RegAddr(src_reg));
            __ movl(RegAddr(dest_reg), rax);
            break;
        case LL_MOVE_I64:
            __ movq(rax, RegAddr(src_reg));
            __ movq(RegAddr(dest_reg), rax);
            break;
        case LL_STOR_S_A:
            __ movl(rax, RegAddr(src_reg));
            EmitIncRefForArrayEscape(rax, rcx);
            __ movl(rdx, RegAddr(dest_reg));
            EmitDecRef(rdx, rax);
            __ movl(RegAddr(dest_reg), rax);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitNewArray(const TypeDesc* td, uint16_t size_reg, uint16_t dest_reg) {
    __ movl(rax, RegAddr(size_reg));
    __ cmpl(rax, 0);
    JumpOnError(less, SP_ERROR_ARRAY_BOUNDS);

    __ movq(ArgReg3, rax);
    __ movq(ArgReg2, reinterpret_cast<intptr_t>(td));
    CallRtForHandle(&Runtime::NewArray, 2, dest_reg);
}

void Compiler::EmitNewFixedArray(const TypeDesc* td, uint16_t dest_reg, uint32_t size) {
    __ movl(ArgReg3, size);
    __ movq(ArgReg2, reinterpret_cast<intptr_t>(td));
    CallRtForHandle(&Runtime::NewArray, 2, dest_reg);
}

void Compiler::EmitNewBulkArray(uint8_t dims, const TypeDesc* td, uint16_t size_reg,
                                uint16_t dest_reg)
{
    __ lea(HelperArgReg4, RegAddr(size_reg));
    __ movl(ArgReg3, dims);
    __ movq(ArgReg2, reinterpret_cast<intptr_t>(td));
    CallRtForHandle(&Runtime::NewBulkArray, 3, dest_reg);
}

void Compiler::EmitNewObj(const TypeDesc* td, uint16_t dest_reg) {
    __ movq(ArgReg2, reinterpret_cast<intptr_t>(td));
    CallRtForHandle(&Runtime::NewObject, 1, dest_reg);
}

void Compiler::EmitNewClosure(const TypeDesc* closure_td, MethodInfo* method, uint16_t dest_reg) {
    __ movq(ArgReg3, reinterpret_cast<intptr_t>(method));
    __ movq(ArgReg2, reinterpret_cast<intptr_t>(closure_td));
    CallRtForHandle(&Runtime::NewClosure, 2, dest_reg);
}

void Compiler::EmitAddRef(uint16_t reg) {
    __ movl(rax, RegAddr(reg));
    EmitIncRef(rax);
}

void Compiler::EmitRelease(uint16_t reg) {
    __ movl(rax, RegAddr(reg));
    EmitDecRef(rax, {}, {RegAddr(reg)});
}

void Compiler::EmitCmpI64(LLOp op, uint16_t reg_a, uint16_t reg_b, uint16_t dest) {
    ConditionCode cc = CmpOpToCondition(op);

    __ xorl(rax, rax);
    __ movq(rcx, RegAddr(reg_a));
    __ cmpq(rcx, RegAddr(reg_b));
    __ set(cc, rax);
    __ movl(RegAddr(dest), rax);
}

void Compiler::EmitBinaryI64(LLOp op, uint16_t lhs_reg, uint16_t rhs_reg, uint16_t dest) {
    __ movq(rax, RegAddr(lhs_reg));

    switch (op) {
        case LL_ADD_I64:
            __ addq(rax, RegAddr(rhs_reg));
            break;
        case LL_SMUL_I64:
            __ imulq(rax, RegAddr(rhs_reg));
            break;
        case LL_SUB_I64:
            __ subq(rax, RegAddr(rhs_reg));
            break;
        case LL_XOR_I64:
            __ xorq(rax, RegAddr(rhs_reg));
            break;
        case LL_OR_I64:
            __ orq(rax, RegAddr(rhs_reg));
            break;
        case LL_AND_I64:
            __ andq(rax, RegAddr(rhs_reg));
            break;
        case LL_SHL_I64:
            __ movq(rcx, RegAddr(rhs_reg));
            __ shlq_cl(rax);
            break;
        case LL_SHR_I64:
            __ movq(rcx, RegAddr(rhs_reg));
            __ shrq_cl(rax);
            break;
        case LL_SSHR_I64:
            __ movq(rcx, RegAddr(rhs_reg));
            __ sarq_cl(rax);
            break;
        default:
            assert(false);
    }

    __ movq(RegAddr(dest), rax);
}

void Compiler::EmitUnaryI64(LLOp op, uint16_t src_reg, uint16_t dest_reg) {
    switch (op) {
        case LL_NEG_I64:
            __ movq(rax, RegAddr(src_reg));
            __ negq(rax);
            __ movq(RegAddr(dest_reg), rax);
            break;
        case LL_INVERT_I64:
            __ movq(rax, RegAddr(src_reg));
            __ notq(rax);
            __ movq(RegAddr(dest_reg), rax);
            break;
        case LL_TEST_I64:
            __ movq(rax, RegAddr(src_reg));
            __ testq(rax, rax);
            __ set(not_equal, rax);
            __ movzxb(rax, rax);
            __ movl(RegAddr(dest_reg), rax);
            break;
        case LL_CVT_I64:
            __ movsxd(rax, RegAddr(src_reg));
            __ movq(RegAddr(dest_reg), rax);
            break;
        case LL_TRUNCATE_I64:
            __ movq(rax, RegAddr(src_reg));
            __ movl(RegAddr(dest_reg), rax);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitSdivI64(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) {
    __ movq(rax, RegAddr(lhs));
    __ movq(rcx, RegAddr(rhs));

    __ testq(rcx, rcx);
    JumpOnError(zero, SP_ERROR_DIVIDE_BY_ZERO);

    // A more subtle case; -INT_MIN / -1 yields an overflow exception.
    Label ok;
    __ cmpq(rcx, -1);
    __ j(not_equal, &ok);
    __ movq(r8, (intptr_t)0x8000000000000000ULL);
    __ cmpq(rax, r8);
    JumpOnError(equal, SP_ERROR_INTEGER_OVERFLOW);
    __ bind(&ok);

    __ movq(rdx, rax);
    __ sarq(rdx, 63);
    __ idivq(rcx);

    if (op == LL_SDIV_I64)
        __ movq(RegAddr(dest), rax);
    else if (op == LL_SMOD_I64)
        __ movq(RegAddr(dest), rdx);
    else
        assert(false);
}

void Compiler::EmitLoadInternedObj(uint32_t addr, uint16_t dest_reg) {
    __ movl(rax, addr);
    __ incq(HeapAddr(rax, offsetof(HeapItem, rc)));
    __ movl(RegAddr(dest_reg), rax);
}

void Compiler::EmitLoadI(LLOp op, uint32_t src_reg, uint32_t dest_reg) {
    __ movl(rax, RegAddr(src_reg));
    switch (op) {
        case LL_LOAD_I_I32:
        case LL_LOAD_I_F32:
            __ movl(rdx, HeapAddr(rax));
            __ movl(RegAddr(dest_reg), rdx);
            break;
        case LL_LOAD_I_U8:
            __ movzxb(rax, HeapAddr(rax));
            __ movl(RegAddr(dest_reg), rax);
            break;
        case LL_LOAD_I_I64:
            __ movq(rax, HeapAddr(rax));
            __ movq(RegAddr(dest_reg), rax);
            break;
        case LL_LOAD_I_I8:
            __ movsxb(rax, HeapAddr(rax));
            __ movl(RegAddr(dest_reg), rax);
            break;
        case LL_LOAD_I_I16:
            __ movsxw(rax, HeapAddr(rax));
            __ movl(RegAddr(dest_reg), rax);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitStorI(LLOp op, uint32_t addr_reg, uint32_t val_reg) {
    __ movl(rdx, RegAddr(addr_reg));
    switch (op) {
        case LL_STOR_I_I32:
        case LL_STOR_I_F32:
            __ movl(rax, RegAddr(val_reg));
            __ movl(HeapAddr(rdx), rax);
            break;
        case LL_STOR_I_I8:
            __ movl(rax, RegAddr(val_reg));
            __ movb(HeapAddr(rdx), rax);
            break;
        case LL_STOR_I_I64:
            __ movq(rax, RegAddr(val_reg));
            __ movq(HeapAddr(rdx), rax);
            break;
        case LL_STOR_I_I16:
            __ movl(rax, RegAddr(val_reg));
            __ movw(HeapAddr(rdx), rax);
            break;
        case LL_STOR_I_A:
            __ movl(rax, RegAddr(val_reg));
            EmitIncRefForArrayEscape(rax, rcx);
            __ movl(rcx, HeapAddr(rdx));
            __ movl(HeapAddr(rdx), rax);
            EmitDecRef(rcx, {});
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitLoadFld(LLOp op, uint16_t addr_reg, uint16_t offset, uint16_t dest_reg) {
    __ movl(rdx, RegAddr(addr_reg));
    __ testl(rdx, rdx);
    JumpOnError(zero, SP_ERROR_NULL_DEREF);

    switch (op) {
        case LL_LOAD_FLD_X32:
            __ movl(rax, HeapAddr(rdx, offset));
            __ movl(RegAddr(dest_reg), rax);
            break;
        case LL_LOAD_FLD_X64:
            __ movq(rax, HeapAddr(rdx, offset));
            __ movq(RegAddr(dest_reg), rax);
            break;
        case LL_LOAD_FLD_A:
            __ movl(rax, HeapAddr(rdx, offset));
            EmitIncRef(rax);
            __ movl(RegAddr(dest_reg), rax);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitStorFld(LLOp op, uint16_t addr_reg, uint16_t offset, uint16_t val_reg) {
    __ movl(rdx, RegAddr(addr_reg));
    __ testl(rdx, rdx);
    JumpOnError(zero, SP_ERROR_NULL_DEREF);

    switch (op) {
        case LL_STOR_FLD_X32:
            __ movl(rax, RegAddr(val_reg));
            __ movl(HeapAddr(rdx, offset), rax);
            break;
        case LL_STOR_FLD_X64:
            __ movq(rax, RegAddr(val_reg));
            __ movq(HeapAddr(rdx, offset), rax);
            break;
        case LL_STOR_FLD_A:
            __ movl(rax, RegAddr(val_reg));
            EmitIncRefForArrayEscape(rax, rcx);
            __ movl(rcx, HeapAddr(rdx, offset));
            __ movl(HeapAddr(rdx, offset), rax);
            EmitDecRef(rcx, {});
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitLoadGlb(LLOp op, uint32_t addr, uint16_t dest_reg) {
    if (op == LL_ADDR_GLB) {
        __ movl(RegAddr(dest_reg), addr);
        return;
    }

    __ movl(rdx, addr);
    switch (op) {
        case LL_LOAD_GLB_X32:
            __ movl(rax, HeapAddr(rdx));
            __ movl(RegAddr(dest_reg), rax);
            break;
        case LL_LOAD_GLB_X64:
            __ movq(rax, HeapAddr(rdx));
            __ movq(RegAddr(dest_reg), rax);
            break;
        case LL_LOAD_GLB_A:
            __ movl(rax, HeapAddr(rdx));
            EmitIncRef(rax);
            __ movl(RegAddr(dest_reg), rax);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitStorGlb(LLOp op, uint32_t addr, uint16_t val_reg) {
    __ movl(rdx, addr);

    switch (op) {
        case LL_STOR_GLB_X32:
            __ movl(rax, RegAddr(val_reg));
            __ movl(HeapAddr(rdx), rax);
            break;
        case LL_STOR_GLB_X64:
            __ movq(rax, RegAddr(val_reg));
            __ movq(HeapAddr(rdx), rax);
            break;
        case LL_STOR_GLB_A:
            __ movl(rax, RegAddr(val_reg));
            EmitIncRefForArrayEscape(rax, rcx);
            __ movl(rcx, HeapAddr(rdx));
            __ movl(HeapAddr(rdx), rax);
            EmitDecRef(rcx, {});
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitFillArray(uint16_t addr_reg, const void* data, uint32_t data_size) {
    __ movl(rdi, RegAddr(addr_reg));
    __ movl(rdi, HeapAddr(rdi, offsetof(SpArray, data)));
    __ lea(rdi, HeapAddr(rdi));

    __ movq(rsi, reinterpret_cast<intptr_t>(data));
    if (data_size >= 8) {
        __ movl(rcx, data_size / 8);
        __ rep_movsq();
    }
    if (data_size % 8) {
        __ movl(rcx, data_size % 8);
        __ rep_movsb();
    }
}

void Compiler::EmitFillArrayFlat(uint16_t addr_reg, const void* data_addr, uint32_t data_size) {
    __ movl(rax, RegAddr(addr_reg));
    __ lea(rdi, HeapAddr(rax));
    __ movq(rsi, reinterpret_cast<intptr_t>(data_addr));
    if (data_size >= 8) {
        __ movl(rcx, data_size / 8);
        __ rep_movsq();
    }
    if (data_size % 8) {
        __ movl(rcx, data_size % 8);
        __ rep_movsb();
    }
}

static inline std::optional<Scale> EltSizeToScale(uint32_t elt_size) {
    switch (elt_size) {
        case 1:
            return {NoScale};
        case 2:
            return {ScaleTwo};
        case 4:
            return {ScaleFour};
        case 8:
            return {ScaleEight};
        default:
            return {};
    }
}

void Compiler::EmitIdxAddrFlat(const IdxAddrFlatArgs& op) {
    __ movl(rax, RegAddr(op.index_reg));
    __ cmpl(rax, op.size);

    auto& thunk = AddBoundsErrorThunk();
    thunk.index = rax;
    thunk.limit = op.size;
    __ j(above_equal, &thunk.label);

    auto scale = EltSizeToScale(op.elt_size);
    if (scale) {
        __ movl(rcx, RegAddr(op.base_reg));
        __ lea(rdx, Operand(rcx, rax, *scale));
    } else {
        // :TODO: strength reduction?
        __ imull(rdx, rax, op.elt_size);
        __ addl(rdx, RegAddr(op.base_reg));
    }
    __ movl(RegAddr(op.dest_reg), rdx);
}

void Compiler::EmitLoadElemFlat(LLOp op, const LoadElemFlatArgs& args) {
    __ movl(rcx, RegAddr(args.index_reg));
    __ cmpl(rcx, args.array_size);

    auto& thunk = AddBoundsErrorThunk();
    thunk.index = rcx;
    thunk.limit = args.array_size;
    __ j(above_equal, &thunk.label);

    int32_t base_offset = args.base_reg * sizeof(cell_t);
    switch (op) {
        case LL_LOAD_ELEM_FLAT_I32:
        case LL_LOAD_ELEM_FLAT_F32:
            __ movl(rax, Operand(frm, rcx, ScaleFour, base_offset));
            __ movl(RegAddr(args.dest_reg), rax);
            break;
        case LL_LOAD_ELEM_FLAT_U8:
            __ movzxb(rax, Operand(frm, rcx, NoScale, base_offset));
            __ movl(RegAddr(args.dest_reg), rax);
            break;
        case LL_LOAD_ELEM_FLAT_I8:
            __ movsxb(rax, Operand(frm, rcx, NoScale, base_offset));
            __ movl(RegAddr(args.dest_reg), rax);
            break;
        case LL_LOAD_ELEM_FLAT_I16:
            __ movsxw(rax, Operand(frm, rcx, ScaleTwo, base_offset));
            __ movl(RegAddr(args.dest_reg), rax);
            break;
        case LL_LOAD_ELEM_FLAT_I64:
            __ movq(rax, Operand(frm, rcx, ScaleEight, base_offset));
            __ movq(RegAddr(args.dest_reg), rax);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitLoadElemFlatI(LLOp op, const LoadElemFlatArgs& args) {
    __ movl(rcx, RegAddr(args.index_reg));
    __ cmpl(rcx, args.array_size);

    auto& thunk = AddBoundsErrorThunk();
    thunk.index = rcx;
    thunk.limit = args.array_size;
    __ j(above_equal, &thunk.label);

    __ movl(rdx, RegAddr(args.base_reg));
    __ lea(rdx, HeapAddr(rdx));
    switch (op) {
        case LL_LOAD_ELEM_FLAT_I_I32:
        case LL_LOAD_ELEM_FLAT_I_F32:
            __ movl(rax, Operand(rdx, rcx, ScaleFour));
            __ movl(RegAddr(args.dest_reg), rax);
            break;
        case LL_LOAD_ELEM_FLAT_I_U8:
            __ movzxb(rax, Operand(rdx, rcx, NoScale));
            __ movl(RegAddr(args.dest_reg), rax);
            break;
        case LL_LOAD_ELEM_FLAT_I_I8:
            __ movsxb(rax, Operand(rdx, rcx, NoScale));
            __ movl(RegAddr(args.dest_reg), rax);
            break;
        case LL_LOAD_ELEM_FLAT_I_I16:
            __ movsxw(rax, Operand(rdx, rcx, ScaleTwo));
            __ movl(RegAddr(args.dest_reg), rax);
            break;
        case LL_LOAD_ELEM_FLAT_I_I64:
            __ movq(rax, Operand(rdx, rcx, ScaleEight));
            __ movq(RegAddr(args.dest_reg), rax);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitStorElemFlat(LLOp op, const StorElemFlatArgs& args) {
    __ movl(rcx, RegAddr(args.index_reg));
    __ cmpl(rcx, args.array_size);

    auto& thunk = AddBoundsErrorThunk();
    thunk.index = rcx;
    thunk.limit = args.array_size;
    __ j(above_equal, &thunk.label);

    int32_t base_offset = args.base_reg * sizeof(cell_t);
    switch (op) {
        case LL_STOR_ELEM_FLAT_I32:
        case LL_STOR_ELEM_FLAT_F32:
            __ movl(rax, RegAddr(args.val_reg));
            __ movl(Operand(frm, rcx, ScaleFour, base_offset), rax);
            break;
        case LL_STOR_ELEM_FLAT_I8:
            __ movl(rax, RegAddr(args.val_reg));
            __ movb(Operand(frm, rcx, NoScale, base_offset), rax);
            break;
        case LL_STOR_ELEM_FLAT_I16:
            __ movl(rax, RegAddr(args.val_reg));
            __ movw(Operand(frm, rcx, ScaleTwo, base_offset), rax);
            break;
        case LL_STOR_ELEM_FLAT_I64:
            __ movq(rax, RegAddr(args.val_reg));
            __ movq(Operand(frm, rcx, ScaleEight, base_offset), rax);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitStorElemFlatI(LLOp op, const StorElemFlatArgs& args) {
    __ movl(rcx, RegAddr(args.index_reg));
    __ cmpl(rcx, args.array_size);

    auto& thunk = AddBoundsErrorThunk();
    thunk.index = rcx;
    thunk.limit = args.array_size;
    __ j(above_equal, &thunk.label);

    __ movl(rdx, RegAddr(args.base_reg));
    __ lea(rdx, HeapAddr(rdx));
    switch (op) {
        case LL_STOR_ELEM_FLAT_I_I32:
        case LL_STOR_ELEM_FLAT_I_F32:
            __ movl(rax, RegAddr(args.val_reg));
            __ movl(Operand(rdx, rcx, ScaleFour), rax);
            break;
        case LL_STOR_ELEM_FLAT_I_I8:
            __ movl(rax, RegAddr(args.val_reg));
            __ movb(Operand(rdx, rcx, NoScale), rax);
            break;
        case LL_STOR_ELEM_FLAT_I_I16:
            __ movl(rax, RegAddr(args.val_reg));
            __ movw(Operand(rdx, rcx, ScaleTwo), rax);
            break;
        case LL_STOR_ELEM_FLAT_I_I64:
            __ movq(rax, RegAddr(args.val_reg));
            __ movq(Operand(rdx, rcx, ScaleEight), rax);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitLoadElem(LLOp op, uint16_t base_reg, uint16_t index_reg, uint16_t dest_reg) {
    __ movl(rdx, RegAddr(base_reg));
    __ testl(rdx, rdx);
    JumpOnError(zero, SP_ERROR_NULL_DEREF);

    __ movl(rcx, RegAddr(index_reg));
    __ movl(rax, HeapAddr(rdx, offsetof(SpArray, length)));
    __ cmpl(rcx, rax);

    auto& thunk = AddBoundsErrorThunk();
    thunk.index = rcx;
    thunk.limit = rax;
    __ j(above_equal, &thunk.label);

    __ movl(rdx, HeapAddr(rdx, offsetof(SpArray, data)));
    __ lea(rdx, HeapAddr(rdx));
    switch (op) {
        case LL_LOAD_ELEM_I32:
        case LL_LOAD_ELEM_F32:
            __ movl(rax, Operand(rdx, rcx, ScaleFour));
            __ movl(RegAddr(dest_reg), rax);
            break;
        case LL_LOAD_ELEM_U8:
            __ movzxb(rax, Operand(rdx, rcx, NoScale));
            __ movl(RegAddr(dest_reg), rax);
            break;
        case LL_LOAD_ELEM_I8:
            __ movsxb(rax, Operand(rdx, rcx, NoScale));
            __ movl(RegAddr(dest_reg), rax);
            break;
        case LL_LOAD_ELEM_I16:
            __ movsxw(rax, Operand(rdx, rcx, ScaleTwo));
            __ movl(RegAddr(dest_reg), rax);
            break;
        case LL_LOAD_ELEM_I64:
            __ movq(rax, Operand(rdx, rcx, ScaleEight));
            __ movq(RegAddr(dest_reg), rax);
            break;
        case LL_LOAD_ELEM_A:
            __ movl(rax, Operand(rdx, rcx, ScaleFour));
            EmitIncRef(rax);
            __ movl(RegAddr(dest_reg), rax);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitStorElem(LLOp op, uint16_t base_reg, uint16_t index_reg, uint16_t val_reg) {
    __ movl(rdx, RegAddr(base_reg));
    __ testl(rdx, rdx);
    JumpOnError(zero, SP_ERROR_NULL_DEREF);

    __ movl(rcx, RegAddr(index_reg));
    __ movl(rax, HeapAddr(rdx, offsetof(SpArray, length)));
    __ cmpl(rcx, rax);

    auto& thunk = AddBoundsErrorThunk();
    thunk.index = rcx;
    thunk.limit = rax;
    __ j(above_equal, &thunk.label);

    __ movl(rdx, HeapAddr(rdx, offsetof(SpArray, data)));
    __ lea(rdx, HeapAddr(rdx));
    switch (op) {
        case LL_STOR_ELEM_I32:
        case LL_STOR_ELEM_F32:
            __ movl(rax, RegAddr(val_reg));
            __ movl(Operand(rdx, rcx, ScaleFour), rax);
            break;
        case LL_STOR_ELEM_I8:
            __ movl(rax, RegAddr(val_reg));
            __ movb(Operand(rdx, rcx, NoScale), rax);
            break;
        case LL_STOR_ELEM_I16:
            __ movl(rax, RegAddr(val_reg));
            __ movw(Operand(rdx, rcx, ScaleTwo), rax);
            break;
        case LL_STOR_ELEM_I64:
            __ movq(rax, RegAddr(val_reg));
            __ movq(Operand(rdx, rcx, ScaleEight), rax);
            break;
        case LL_STOR_ELEM_A:
            __ movl(rax, RegAddr(val_reg));
            EmitIncRefForArrayEscape(rax, r8);
            __ lea(rdx, Operand(rdx, rcx, ScaleFour));
            __ movl(rcx, Operand(rdx, 0));
            __ movl(Operand(rdx, 0), rax);
            EmitDecRef(rcx, {});
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitLoadUpvar(LLOp op, const UpvarArgs& args) {
    __ movl(rdx, RegAddr(args.closure_reg));

    switch (op) {
        case LL_ADDR_UPVAR:
            __ lea(rax, Operand(rdx, args.slot));
            __ movl(RegAddr(args.reg), rax);
            break;
        case LL_LOAD_UPVAR_X32:
            __ movl(rax, HeapAddr(rdx, args.slot));
            __ movl(RegAddr(args.reg), rax);
            break;
        case LL_LOAD_UPVAR_X64:
            __ movq(rax, HeapAddr(rdx, args.slot));
            __ movq(RegAddr(args.reg), rax);
            break;
        case LL_LOAD_UPVAR_A:
            __ movl(rax, HeapAddr(rdx, args.slot));
            EmitIncRef(rax);
            __ movl(RegAddr(args.reg), rax);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitStorUpvar(LLOp op, const UpvarArgs& args) {
    __ movl(rdx, RegAddr(args.closure_reg));

    switch (op) {
        case LL_STOR_UPVAR_X32:
            __ movl(rax, RegAddr(args.reg));
            __ movl(HeapAddr(rdx, args.slot), rax);
            break;
        case LL_STOR_UPVAR_X64:
            __ movq(rax, RegAddr(args.reg));
            __ movq(HeapAddr(rdx, args.slot), rax);
            break;
        case LL_STOR_UPVAR_A:
            __ movl(rax, RegAddr(args.reg));
            EmitIncRefForArrayEscape(rax, rcx);
            __ movl(rcx, HeapAddr(rdx, args.slot));
            __ movl(HeapAddr(rdx, args.slot), rax);
            EmitDecRef(rcx, {});
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitSlice(uint16_t base_reg, uint16_t index_reg, uint16_t dest_reg) {
    __ movl(ArgReg2, RegAddr(base_reg));
    __ testl(ArgReg2, ArgReg2);
    JumpOnError(zero, SP_ERROR_NULL_DEREF);

    __ movl(ArgReg3, RegAddr(index_reg));
    __ lea(ArgReg2, HeapAddr(ArgReg2));
    CallRtForHandle(&Runtime::NewSlice, 2, dest_reg);
}

void Compiler::EmitSliceEs(uint16_t src_reg, uint16_t dest_reg, uint32_t cells) {
    __ movl(ArgReg3, cells);
    __ movl(ArgReg2, RegAddr(src_reg));
    CallRtForHandle(&Runtime::NewSliceEs, 2, dest_reg);
}

void Compiler::EmitSliceFlat(const SliceFlatArgs& op) {
    __ movq(HelperArgReg4, RegAddr(op.index_reg));
    __ movq(ArgReg3, reinterpret_cast<intptr_t>(op.td));
    __ movl(ArgReg2, RegAddr(op.base_reg));

    CallRtForHandle(&Runtime::NewFlatSlice, 3, op.dest_reg);
}

void Compiler::EmitIdxAddr(const IdxAddrArgs& op) {
    __ movl(rax, RegAddr(op.base_reg));
    __ testl(rax, rax);
    JumpOnError(zero, SP_ERROR_NULL_DEREF);

    __ movl(rcx, RegAddr(op.index_reg));
    __ movl(rdx, HeapAddr(rax, offsetof(SpArray, length)));

    auto& thunk = AddBoundsErrorThunk();
    thunk.index = rcx;
    thunk.limit = rdx;
    __ cmpl(rcx, rdx);
    __ j(above_equal, &thunk.label);

    __ movl(rax, HeapAddr(rax, offsetof(SpArray, data)));
    if (auto scale = EltSizeToScale(op.elt_size)) {
        __ lea(rax, Operand(rax, rcx, *scale));
    } else {
        __ imull(rdx, rcx, op.elt_size);
        __ addl(rax, rdx);
    }
    __ movl(RegAddr(op.dest_reg), rax);
}

void Compiler::EmitCopyArray(LLOp op, uint16_t src_reg, uint16_t dest_reg, uint32_t bytes) {
    __ movl(rsi, RegAddr(src_reg));
    __ movl(rdi, RegAddr(dest_reg));

    if (op == LL_COPYARRAY) {
        __ testl(rsi, rsi);
        JumpOnError(zero, SP_ERROR_NULL_DEREF);
        __ movl(rsi, HeapAddr(rsi, offsetof(SpArray, data)));

        __ testl(rdi, rdi);
        JumpOnError(zero, SP_ERROR_NULL_DEREF);
        __ movl(rdi, HeapAddr(rdi, offsetof(SpArray, data)));
    }

    __ lea(rsi, HeapAddr(rsi));
    __ lea(rdi, HeapAddr(rdi));

    if (bytes >= 8) {
        __ movl(rcx, bytes / 8);
        __ rep_movsq();
    }
    if (bytes % 8) {
        __ movl(rcx, bytes % 8);
        __ rep_movsb();
    }
}

void Compiler::EmitCopyObj(uint16_t src_reg, uint16_t dest_reg, uint32_t bytes) {
    assert(bytes % 4 == 0);

    __ movl(rdi, RegAddr(dest_reg));
    __ lea(rdi, HeapAddr(rdi));
    __ movl(rsi, RegAddr(src_reg));
    __ lea(rsi, HeapAddr(rsi));
    if (bytes >= 8) {
        __ movl(rcx, bytes / 8);
        __ rep_movsq();
    }
    // :TODO: validate
    if (bytes % 8 == 4) {
        __ movsd();
    }
}

void Compiler::EmitArrayToFlat(uint16_t src_reg, uint16_t dest_reg) {
    __ movl(rdx, RegAddr(src_reg));
    __ testl(rdx, rdx);
    JumpOnError(zero, SP_ERROR_NULL_DEREF);

    __ movl(rax, HeapAddr(rdx, offsetof(SpArray, data)));
    __ movl(RegAddr(dest_reg), rax);
}

void Compiler::EmitAddrFld(uint16_t src_reg, uint16_t dest_reg, uint32_t offset) {
    __ movl(rax, RegAddr(src_reg));
    __ testl(rax, rax);
    JumpOnError(zero, SP_ERROR_NULL_DEREF);

    __ lea(rax, Operand(rax, offset));
    __ movl(RegAddr(dest_reg), rax);
}

void Compiler::EmitSwitchChain(uint16_t val_reg, uint32_t def_block, const std::span<const SwitchCaseEntry>& cases) {
    __ movl(rax, RegAddr(val_reg));
    for (size_t i = 0; i < cases.size(); i++) {
        const auto& entry = cases[i];

        __ cmpl(rax, entry.value);
        __ j(equal, &block_labels_[block_->successors[i + 1]]);
    }
    __ jmp(&block_labels_[def_block]);
}

void Compiler::EmitSwitchTable(uint16_t val_reg, uint32_t def_block, const std::span<const SwitchCaseEntry>& cases) {
    __ movl(rcx, RegAddr(val_reg));

    cell_t low = cases[0].value;
    if (low != 0) {
        low = -low;
        __ lea(rcx, Operand(rcx, low));
    }

    cell_t high = abs(cases[0].value - cases.back().value);
    __ cmpl(rcx, high);
    __ j(above, &block_labels_[def_block]);

    RipDataLabel table;
    __ lea(rdx, &table);
    __ movsxd(rcx, rcx);
    __ jmp(Operand(rdx, rcx, ScaleEight));

    // We emit absolute addresses in reverse order in the assembler, to
    // avoid rip-relative fixups during linking. This means we have to
    // walk the case statements backwards, to make sure the table can
    // be read in forward order.
    for (size_t i = cases.size() - 1; i < cases.size(); i--) {
        uint32_t target = block_->successors[i + 1];
        __ emit_absolute_address(&block_addresses_[target]);
    }
    __ bind(&table);
}

void Compiler::EmitCallee(uint16_t dest_reg) {
    __ movl(rax, Operand(rbp, kCalleeSlotOffset));
    __ movl(RegAddr(dest_reg), rax);
}

void Compiler::EmitGetFuncId(uint16_t src_reg, uint16_t dest_reg) {
    __ movl(rdx, RegAddr(src_reg));
    __ testl(rdx, rdx);
    JumpOnError(zero, SP_ERROR_NULL_DEREF);

    __ movq(rdx, HeapAddr(rdx, offsetof(SpFunction, method)));
    __ testq(rdx, rdx);
    JumpOnError(zero, SP_ERROR_NULL_DEREF);

    __ movl(rax, Operand(rdx, MethodInfo::offsetOfMethodIndex()));
    __ shll(rax, 1);
    __ orl(rax, 1);
    __ movl(RegAddr(dest_reg), rax);
}

void Compiler::EmitIncRefForArrayEscape(Register obj_reg, Register tmp_reg) {
    Label done;
    __ testl(obj_reg, obj_reg);
    __ j(zero, &done);
    __ movq(tmp_reg, HeapAddr(obj_reg, offsetof(HeapItem, td)));
    __ movzxb(tmp_reg, Operand(tmp_reg, TypeDesc::OffsetOfKind()));
    __ cmpl(tmp_reg, static_cast<uint8_t>(TypeKind::ArraySlice));
    JumpOnError(equal, SP_ERROR_SLICE_ESCAPE);
    __ incq(HeapAddr(obj_reg, offsetof(HeapItem, rc)));
    __ bind(&done);
}

void Compiler::EmitIncRef(Register obj_reg) {
    Label done;
    __ testl(obj_reg, obj_reg);
    __ j(zero, &done);
    __ incq(HeapAddr(obj_reg, offsetof(HeapItem, rc)));
    __ bind(&done);
}

void Compiler::EmitDecRef(Register obj_reg, std::optional<Register> save_reg,
                          const std::optional<Operand>& zero_loc)
{
    dealloc_thunks_.emplace_back(obj_reg, save_reg, op_cip_);
    auto& thunk = dealloc_thunks_.back();

    __ testl(obj_reg, obj_reg);
    __ j(zero, &thunk.return_label);
    if (zero_loc)
        __ movl(*zero_loc, 0);
    __ decq(HeapAddr(obj_reg, offsetof(HeapItem, rc)));
    __ j(zero, &thunk.label);
    __ bind(&thunk.return_label);
}

void Compiler::CallRtForHandleImpl(void* method_addr, uint32_t nargs, uint16_t dest_reg) {
    assert(nargs <= 3);

#ifdef _WIN64
    if (nargs >= 3)
        __ movq(Operand(rsp, 32), HelperArgReg4);
#endif

    __ movq(ArgReg1, context_reg);
    __ lea(ArgReg0, Operand(rbp, kHandleOffset));

    // Clear exit_fp_ so DispatchReport defers the error.
    __ xorq(rax, rax);
    __ movq(Operand(env_reg, Environment::offsetOfExit()), rax);

    __ callWithABI(ExternalAddress(method_addr));
    __ movq(rax, Operand(rbp, kHandleOffset));

    auto& thunk = AddDeferredErrorThunk();
    __ testq(rax, rax);
    __ j(zero, &thunk.label);

    __ subq(rax, dat_reg);
    __ movl(RegAddr(dest_reg), rax);
}

void Compiler::EmitDeallocThunk(DeallocThunk* thunk) {
    if (thunk->save_reg)
        __ movq(Operand(rsp, 0), *thunk->save_reg);

    __ lea(ArgReg0, HeapAddr(thunk->obj_reg));
    __ callWithABI(ExternalAddress(env_->stubs()->DeallocStub()));
    EmitCipMapping(thunk->cip);

    if (thunk->save_reg)
        __ movq(*thunk->save_reg, Operand(rsp, 0));

    __ jmp(&thunk->return_label);
}

void Compiler::EmitBoundsErrorThunk(BoundsErrorThunk* thunk) {
    if (std::holds_alternative<Register>(thunk->limit))
        __ movl(Operand(rsp, 4), std::get<Register>(thunk->limit));
    else
        __ movl(Operand(rsp, 4), std::get<uint32_t>(thunk->limit));

    if (std::holds_alternative<Register>(thunk->index))
        __ movl(Operand(rsp, 0), std::get<Register>(thunk->index));
    else
        __ movl(Operand(rsp, 0), std::get<uint32_t>(thunk->index));

    __ call(ExternalAddress(env_->stubs()->return_stubs_v2().bounds_error));
    EmitCipMapping(thunk->cip);
}

void Compiler::EmitDeferredErrorThunk(DeferredErrorThunk* thunk) {
    __ call(ExternalAddress(stubs_.deferred_error));
    EmitCipMapping(thunk->cip);
}

void Compiler::EmitCallThunk(CallThunk* thunk) {
    // Get the return address, since that is the call that we need to patch.
    __ movq(ArgReg2, Operand(rsp, 0));

    __ setupExitFrame(ExitFrameType::Helper, 0);

    __ movq(ArgReg1, thunk->method_index);
    __ movq(ArgReg0, context_reg);
    __ callWithABI(ExternalAddress((void*)LazyCompileThunk));
    __ leaveExitFrame();

    __ testq(rax, rax);
    JumpOnReportedError(zero);

    __ jmp(rax);
}

void Compiler::JumpOnError(ConditionCode cc, int err) {
    error_thunks_.emplace_back(op_cip_, err);
    __ j(cc, &error_thunks_.back().label);
}

void Compiler::JumpOnReportedError(ConditionCode cc) {
    error_thunks_.emplace_back(op_cip_, -1);
    __ j(cc, &error_thunks_.back().label);
}

} // namespace sp::v2
