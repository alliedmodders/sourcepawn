// vim: set ts=4 sw=4 tw=99 et:
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
//
#include "jit_x86.h"
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "binary-reader.h"
#include "v2/lowering/ll-op.h"
#include "v2/lowering/llcode.h"
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

Compiler::Compiler(Runtime* rt, MethodInfo* method)
 : CompilerBase(rt, method) {
}

Compiler::~Compiler() {
}

void CompilerBase::PatchCallThunk(uint8_t* pc, void* target) {
    *(intptr_t*)(pc - 4) = intptr_t(target) - intptr_t(pc);
}

bool CompilerBase::IsSupported() {
    return false && FeaturesX86::Get().fpu && FeaturesX86::Get().sse && FeaturesX86::Get().sse2;
}

bool CompilerBase::SupportsPlugin(Runtime* cx) {
    return true;
}

// Every JIT function gets 16 bytes of stack, plus an extra 12 for alignment on CCC.
static constexpr int kNativeStackAllowance = (3 + 4) * sizeof(intptr_t);

// Handle<> storage is placed at the top of the pre-allocated stack area
// (ebp-16 = esp+24). On SysV this leaves 6 slots (esp+0..esp+20) for the
// hidden return pointer, |this|, and up to 4 method arguments.
static constexpr int kHandleOffset = -16;

void Compiler::EmitPrologue(const FrameInfo& frame) {
    __ enterFrame(JitFrameType::Scripted, method_info_->frame_id());

    __ push(frm);
    __ subl(esp, kNativeStackAllowance);
    __ movl(frm, stk);

    if (frame.frame_size) {
        __ movl(eax, frm);
        __ addl(eax, frame.frame_size);

        __ movl(ecx, Operand(ExternalAddress(env_->addressOfSpTop())));
        __ cmpl(eax, ecx);
        JumpOnError(above, SP_ERROR_STACKLOW);
    }

    if (frame.callee_regs > 0) {
        __ push(edi);
        __ xorl(eax, eax);
        __ movl(ecx, frame.callee_regs);
        __ lea(edi, Operand(frm, frame.num_params * sizeof(cell_t)));
        __ rep_stosd();
        __ pop(edi);
    }

    // Set stk = frm + num_regs * 4.
    __ lea(stk, Operand(frm, int32_t(frame.num_regs * sizeof(cell_t))));
    __ movl(Operand(ExternalAddress(env_->addressOfSp())), stk);
}

void Compiler::EmitCallThunk(CallThunk* thunk) {
    // Get the return address, since that is the call that we need to patch.
    __ movl(eax, Operand(esp, 0));

    // Enter the exit frame. This aligns the stack.
    __ enterExitFrame(ExitFrameType::Helper, 0);

    static const size_t kStackNeeded = 3 * sizeof(void*);
    static const size_t kStackReserve = ke::Align(kStackNeeded, 16);
    __ subl(esp, kStackReserve);

    // Set arguments.
    __ movl(Operand(esp, 2 * sizeof(void*)), eax);
    __ movl(Operand(esp, 1 * sizeof(void*)), thunk->method_index);
    __ movl(Operand(esp, 0 * sizeof(void*)), intptr_t(context_));

    __ callWithABI(ExternalAddress((void*)LazyCompileThunk));
    __ leaveExitFrame();

    __ testl(eax, eax);
    __ j(zero, ExternalAddress(stubs_.return_reported_error));

    __ jmp(eax);
}

void Compiler::JumpOnError(ConditionCode cc, int err) {
    error_thunks_.emplace_back(op_cip_, err);
    __ j(cc, &error_thunks_.back().label);
}

void Compiler::JumpAndReportOnError(ConditionCode cc) {
    error_thunks_.emplace_back(op_cip_, 0);
    __ j(cc, &error_thunks_.back().label);
}

void Compiler::JumpOnReportedError(ConditionCode cc) {
    error_thunks_.emplace_back(op_cip_, -1);
    __ j(cc, &error_thunks_.back().label);
}

void Compiler::EmitLoadConst(uint16_t reg, cell_t val) {
    __ movl(RegAddr(reg), val);
}

void Compiler::EmitLoadConst64(uint16_t reg, int64_t val) {
    Int64CellUnion u(val);
    __ movl(RegAddr(reg), u.cells[0]);
    __ movl(RegAddr(reg + 1), u.cells[1]);
}

void Compiler::EmitAddr(uint16_t src_reg, uint16_t dest_reg) {
    __ lea(eax, RegAddr(src_reg));
    __ movl(RegAddr(dest_reg), eax);
}

void Compiler::EmitRetn(LLOp op, std::optional<uint16_t> reg) {
    if (reg)
        __ movl(eax, RegAddr(*reg));
    else
        __ xorl(eax, eax);

    if (op == LL_RETN_A)
        EmitIncRefForArrayEscape(eax, ecx);

    // Restore callee-saved registers.
    __ movl(stk, frm);
    __ lea(esp, Operand(ebp, -3 * sizeof(intptr_t)));
    __ pop(frm);

    __ leaveFrame();
    __ ret();
}

void Compiler::EmitNativeCall(uint32_t native_index, uint8_t nargs, uint16_t dest,
                              const std::vector<uint16_t>& args, uint16_t spread_reg)
{
    NativeEntry* native = rt_->NativeAt(native_index);

    // Save the old stk value.
    __ movl(Operand(esp, 0), stk);

    __ movl(Operand(stk, 0), nargs);
    for (uint8_t i = 0; i < nargs; i++) {
        uint16_t arg_reg = args[i];
        __ movl(eax, RegAddr(arg_reg));
        __ movl(Operand(stk, (i + 1) * sizeof(cell_t)), eax);
    }

    // Update stk.
    __ lea(stk, Operand(stk, (nargs + 1) * sizeof(cell_t)));

    if (spread_reg != LL_INVALID_REG) {
        __ movl(edx, RegAddr(spread_reg));
        __ movl(ecx, Operand(edx, 0));

        // Check for stack overflow.
        __ lea(eax, Operand(stk, ecx, ScaleFour));
        __ cmpl(eax, Operand(ExternalAddress(env_->addressOfSpTop())));
        JumpOnError(above_equal, SP_ERROR_STACKLOW);

        // Copy arguments. stk (edi) is already positioned to where we need,
        // and ecx already contains the arg count.
        static_assert(stk == edi);
        __ lea(esi, Operand(edx, sizeof(cell_t)));
        __ rep_movsd();

        __ movl(stk, eax);
    }

    // Update stack value.
    __ movl(Operand(ExternalAddress(env_->addressOfSp())), stk);

    // Get |params| back.
    __ movl(eax, Operand(esp, 0));

    CodeLabel return_address;
    __ pushInlineExitFrame(ExitFrameType::Native, native_index, &return_address);

    __ subl(esp, 16);
    __ movl(Operand(esp, 8), eax);
    __ movl(Operand(esp, 4), intptr_t(native));
    __ movl(Operand(esp, 0), intptr_t(context_));

    __ callWithABI(ExternalAddress((void*)NativeInvokeThunk));
    __ bind(&return_address);
    EmitCipMapping(op_cip_);

    __ popInlineExitFrame(4); // 4 was our alignment amount.

    // Restore stack.
    __ movl(stk, Operand(esp, 0));
    __ movl(Operand(ExternalAddress(env_->addressOfSp())), stk);

    // Check for exception.
    __ cmpl(Operand(ExternalAddress(env_->addressOfExceptionCode())), 0);
    __ j(not_zero, ExternalAddress(stubs_.return_reported_error));

    if (dest != 0xFFFF)
        __ movl(RegAddr(dest), eax);
}

void Compiler::EmitScriptedCall(uint32_t method_index, uint8_t nargs, uint16_t dest,
                                const std::vector<uint16_t>& args)
{
    for (uint8_t i = 0; i < nargs; i++) {
        uint16_t arg_reg = args[i];
        __ movl(eax, RegAddr(arg_reg));
        __ movl(Operand(stk, i * sizeof(cell_t)), eax);
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

    if (dest != 0xFFFF)
        __ movl(RegAddr(dest), eax);
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
            return equal;
        case LL_JNEQ:
        case LL_NEQ_I32:
            return not_equal;
        case LL_JSLESS:
        case LL_SLESS_I32:
            return less;
        case LL_JSLEQ:
        case LL_SLEQ_I32:
            return less_equal;
        case LL_JSGRTR:
        case LL_SGRTR_I32:
            return greater;
        case LL_JSGEQ:
        case LL_SGEQ_I32:
            return greater_equal;
        default:
            assert(false);
            return equal;
    }
}

void Compiler::EmitJumpCmp(LLOp op, uint16_t reg_a, uint16_t reg_b, size_t target_idx) {
    ConditionCode cc = CmpOpToCondition(op);

    __ movl(eax, RegAddr(reg_a));
    __ cmpl(eax, RegAddr(reg_b));

    if (IsBlockEmitted(target_idx)) {
        __ j32(cc, &block_labels_[target_idx]);
        backward_jumps_.push_back(BackwardJump(masm.pc(), op_cip_));
    } else {
        __ j(cc, &block_labels_[target_idx]);
    }
}

void Compiler::EmitCmpI32(LLOp op, uint16_t reg_a, uint16_t reg_b, uint16_t dest) {
    ConditionCode cc = CmpOpToCondition(op);

    __ xorl(eax, eax);
    __ movl(ecx, RegAddr(reg_a));
    __ cmpl(ecx, RegAddr(reg_b));
    __ set(cc, eax);
    __ movl(RegAddr(dest), eax);
}

void Compiler::EmitBasicAlu(LLOp op, uint16_t lhs_reg, uint16_t rhs_reg, uint16_t dest) {
    __ movl(eax, RegAddr(lhs_reg));

    switch (op) {
        case LL_ADD_I32:
            __ addl(eax, RegAddr(rhs_reg));
            break;
        case LL_SMUL_I32:
            __ imull(eax, RegAddr(rhs_reg));
            break;
        case LL_SUB_I32:
            __ subl(eax, RegAddr(rhs_reg));
            break;
        case LL_XOR_I32:
            __ xorl(eax, RegAddr(rhs_reg));
            break;
        case LL_OR_I32:
            __ orl(eax, RegAddr(rhs_reg));
            break;
        case LL_AND_I32:
            __ andl(eax, RegAddr(rhs_reg));
            break;
        case LL_SHL_I32:
            __ movl(ecx, RegAddr(rhs_reg));
            __ shll_cl(eax);
            break;
        case LL_SHR_I32:
            __ movl(ecx, RegAddr(rhs_reg));
            __ shrl_cl(eax);
            break;
        case LL_SSHR_I32:
            __ movl(ecx, RegAddr(rhs_reg));
            __ sarl_cl(eax);
            break;
        default:
            assert(false);
    }

    __ movl(RegAddr(dest), eax);
}

void Compiler::EmitUnaryAlu(LLOp op, uint16_t src_reg, uint16_t dest_reg) {
    __ movl(eax, RegAddr(src_reg));

    switch (op) {
        case LL_INVERT_I32:
            __ notl(eax);
            break;
        case LL_NEG_I32:
            __ negl(eax);
            break;
        case LL_NOT_I32:
            __ testl(eax, eax);
            __ movl(eax, 0);
            __ set(zero, r8_al);
            break;
        case LL_TEST_I32:
            __ testl(eax, eax);
            __ movl(eax, 0);
            __ set(not_zero, r8_al);
            break;
        default:
            assert(false);
    }

    __ movl(RegAddr(dest_reg), eax);
}

void Compiler::EmitSdivI32(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) {
    __ movl(eax, RegAddr(lhs));
    __ movl(ecx, RegAddr(rhs));

    __ testl(ecx, ecx);
    JumpOnError(zero, SP_ERROR_DIVIDE_BY_ZERO);

    // A more subtle case; -INT_MIN / -1 yields an overflow exception.
    Label ok;
    __ cmpl(ecx, -1);
    __ j(not_equal, &ok);
    __ cmpl(eax, 0x80000000);
    JumpOnError(equal, SP_ERROR_INTEGER_OVERFLOW);
    __ bind(&ok);

    __ movl(edx, eax);
    __ sarl(edx, 31);
    __ idivl(ecx);

    if (op == LL_SDIV_I32)
        __ movl(RegAddr(dest), eax);
    else if (op == LL_SMOD_I32)
        __ movl(RegAddr(dest), edx);
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
        __ movl(eax, (cc == equal) ? 0 : 1);
        __ j(parity, &done);
        __ set(cc, r8_al);
        __ bind(&done);
    } else {
        __ movl(eax, 0);
        __ set(cc, r8_al);
    }
    __ movl(RegAddr(dest), eax);
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
            __ movl(eax, RegAddr(rhs));
            __ movl(Operand(esp, 4), eax);
            __ movl(eax, RegAddr(lhs));
            __ movl(Operand(esp, 0), eax);
            __ callWithABI(ExternalAddress((void*)::fmodf));
            __ fstp32(RegAddr(dest));
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
            __ movl(RegAddr(dest_reg), eax);
            break;
        case LL_NEG_F32:
            __ movl(eax, RegAddr(src_reg));
            __ movl(ecx, 0x80000000);
            __ xorl(eax, ecx);
            __ movl(RegAddr(dest_reg), eax);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitMove(LLOp op, uint16_t src_reg, uint16_t dest_reg) {
    switch (op) {
        case LL_MOVE:
            __ movl(eax, RegAddr(src_reg));
            __ movl(RegAddr(dest_reg), eax);
            break;
        case LL_MOVE_I64:
            __ movq(xmm0, RegAddr(src_reg));
            __ movq(RegAddr(dest_reg), xmm0);
            break;
        case LL_STOR_S_A:
            __ movl(eax, RegAddr(src_reg));
            EmitIncRefForArrayEscape(eax, ecx);
            __ movl(edx, RegAddr(dest_reg));
            EmitDecRef(edx, eax);
            __ movl(RegAddr(dest_reg), eax);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitNewArray(const TypeDesc* td, uint16_t size_reg, uint16_t dest_reg) {
    __ movl(eax, RegAddr(size_reg));
    __ cmpl(eax, 0);
    JumpOnError(less, SP_ERROR_ARRAY_BOUNDS);

#if defined(_WIN32)
    __ push(eax);
    __ push(reinterpret_cast<intptr_t>(td));
#else
    __ movl(Operand(esp, 12), eax);
    __ movl(Operand(esp, 8), reinterpret_cast<intptr_t>(td));
#endif
    CallRtForHandle(PmfCast<void*>(&Runtime::NewArray), dest_reg);
}

void Compiler::EmitNewFixedArray(const TypeDesc* td, uint16_t dest_reg, uint32_t size) {
#if defined(_WIN32)
    __ push(size);
    __ push(reinterpret_cast<intptr_t>(td));
#else
    __ movl(Operand(esp, 12), size);
    __ movl(Operand(esp, 8), reinterpret_cast<intptr_t>(td));
#endif
    CallRtForHandle(PmfCast<void*>(&Runtime::NewArray), dest_reg);
}

void Compiler::EmitNewBulkArray(uint8_t dims, const TypeDesc* td, uint16_t size_reg,
                                uint16_t dest_reg)
{
    __ lea(eax, RegAddr(size_reg));

#if defined(_WIN32)
    __ push(eax);
    __ push(dims);
    __ push(reinterpret_cast<intptr_t>(td));
#else
    __ movl(Operand(esp, 16), eax);
    __ movl(Operand(esp, 12), dims);
    __ movl(Operand(esp, 8), reinterpret_cast<intptr_t>(td));
#endif
    CallRtForHandle(PmfCast<void*>(&Runtime::NewBulkArray), dest_reg);
}

void Compiler::EmitIncRef(Register obj_reg) {
    Label done;
    __ testl(obj_reg, obj_reg);
    __ j(zero, &done);
    __ incl(Operand(obj_reg, offsetof(HeapItem, rc)));
    __ bind(&done);
}

void Compiler::EmitIncRefForArrayEscape(Register obj_reg, Register tmp_reg) {
    Label done;
    __ testl(obj_reg, obj_reg);
    __ j(zero, &done);
    __ movl(tmp_reg, Operand(obj_reg, offsetof(HeapItem, td)));
    __ movzxb(tmp_reg, Operand(tmp_reg, TypeDesc::OffsetOfKind()));
    __ cmpl(tmp_reg, static_cast<uint8_t>(TypeKind::ArraySlice));
    JumpOnError(equal, SP_ERROR_SLICE_ESCAPE);
    __ incl(Operand(obj_reg, offsetof(HeapItem, rc)));
    __ bind(&done);
}

void Compiler::EmitAddRef(uint16_t reg) {
    __ movl(eax, RegAddr(reg));
    EmitIncRef(eax);
}

void Compiler::EmitRelease(uint16_t reg) {
    __ movl(eax, RegAddr(reg));
    EmitDecRef(eax, {}, {RegAddr(reg)});
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
    __ decl(Operand(obj_reg, offsetof(HeapItem, rc)));
    __ j(zero, &thunk.label);
    __ bind(&thunk.return_label);
}

void Compiler::EmitCmpI64(LLOp op, uint16_t reg_a, uint16_t reg_b, uint16_t dest) {
    switch (op) {
        case LL_EQ_I64:
        case LL_NEQ_I64:
            __ movl(eax, RegAddr(reg_b));
            __ movl(edx, RegAddr(reg_b + 1));
            __ xorl(eax, RegAddr(reg_a));
            __ xorl(edx, RegAddr(reg_a + 1));
            __ orl(eax, edx);
            __ set(op == LL_EQ_I64 ? equal : not_equal, eax);
            break;
        case LL_SLESS_I64:
        case LL_SGRTR_I64:
            if (op == LL_SGRTR_I64)
                std::swap(reg_a, reg_b);

            __ movl(eax, RegAddr(reg_a));
            __ movl(edx, RegAddr(reg_a + 1));
            __ cmpl(eax, RegAddr(reg_b));
            __ sbbl(edx, RegAddr(reg_b + 1));
            __ set(less, eax);
            break;
        case LL_SLEQ_I64:
        case LL_SGEQ_I64:
            if (op == LL_SGEQ_I64)
                std::swap(reg_a, reg_b);

            __ movl(eax, RegAddr(reg_b));
            __ movl(edx, RegAddr(reg_b + 1));
            __ cmpl(eax, RegAddr(reg_a));
            __ sbbl(edx, RegAddr(reg_a + 1));
            __ set(greater_equal, eax);
            break;
        default:
            assert(false);
    }

    __ movzxb(eax, eax);
    __ movl(RegAddr(dest), eax);
}

void Compiler::EmitBinaryI64(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) {
    switch (op) {
        case LL_ADD_I64: {
            __ movq(xmm0, RegAddr(lhs));
            __ movq(xmm1, RegAddr(rhs));
            __ paddq(xmm0, xmm1);
            __ movq(RegAddr(dest), xmm0);
            break;
        }
        case LL_SUB_I64: {
            __ movq(xmm0, RegAddr(lhs));
            __ movq(xmm1, RegAddr(rhs));
            __ psubq(xmm0, xmm1);
            __ movq(RegAddr(dest), xmm0);
            break;
        }
        case LL_SMUL_I64: {
            __ push(edi);

            __ movl(edi, RegAddr(lhs + 1));
            __ movl(ecx, RegAddr(rhs + 1));
            __ imull(edi, RegAddr(rhs));
            __ imull(ecx, RegAddr(lhs));
            __ movl(eax, RegAddr(lhs));
            __ mull(RegAddr(rhs));
            __ addl(ecx, edi);
            __ addl(edx, ecx);

            __ movl(RegAddr(dest), eax);
            __ movl(RegAddr(dest + 1), edx);

            __ pop(edi);
            break;
        }
        case LL_SHL_I64: {
            __ movl(eax, RegAddr(lhs));
            __ movl(edx, RegAddr(lhs + 1));
            __ movl(ecx, RegAddr(rhs));
            __ shld(edx, eax);
            __ shll_cl(eax);

            Label done;
            __ testb(ecx, 0x20);
            __ j(equal, &done);
            __ movl(edx, eax);
            __ xorl(eax, eax);
            __ bind(&done);

            __ movl(RegAddr(dest), eax);
            __ movl(RegAddr(dest + 1), edx);
            break;
        }  
        case LL_SSHR_I64: {
            __ movl(eax, RegAddr(lhs));
            __ movl(edx, RegAddr(lhs + 1));
            __ movl(ecx, RegAddr(rhs));
            __ shrd(eax, edx);
            __ sarl_cl(edx);

            Label done;
            __ testb(ecx, 0x20);
            __ j(equal, &done);
            __ movl(eax, edx);
            __ sarl(edx, 0x1f);
            __ bind(&done);

            __ movl(RegAddr(dest), eax);
            __ movl(RegAddr(dest + 1), edx);
            break;
        }
        case LL_SHR_I64: {
            __ movl(eax, RegAddr(lhs));
            __ movl(edx, RegAddr(lhs + 1));
            __ movl(ecx, RegAddr(rhs));
            __ shrd(eax, edx);
            __ shrl_cl(edx);

            Label done;
            __ testb(ecx, 0x20);
            __ j(equal, &done);
            __ movl(eax, edx);
            __ xorl(edx, edx);
            __ bind(&done);

            __ movl(RegAddr(dest), eax);
            __ movl(RegAddr(dest + 1), edx);
            break;
        }
        case LL_OR_I64: {
            __ movl(eax, RegAddr(lhs));
            __ orl(eax, RegAddr(rhs));
            __ movl(edx, RegAddr(lhs + 1));
            __ orl(edx, RegAddr(rhs + 1));

            __ movl(RegAddr(dest), eax);
            __ movl(RegAddr(dest + 1), edx);
            break;
        }
        case LL_XOR_I64: {
            __ movl(eax, RegAddr(lhs));
            __ xorl(eax, RegAddr(rhs));
            __ movl(edx, RegAddr(lhs + 1));
            __ xorl(edx, RegAddr(rhs + 1));

            __ movl(RegAddr(dest), eax);
            __ movl(RegAddr(dest + 1), edx);
            break;
        }
        case LL_AND_I64: {
            __ movl(eax, RegAddr(lhs));
            __ andl(eax, RegAddr(rhs));
            __ movl(edx, RegAddr(lhs + 1));
            __ andl(edx, RegAddr(rhs + 1));

            __ movl(RegAddr(dest), eax);
            __ movl(RegAddr(dest + 1), edx);
            break;
        }
        default:
            assert(false);
    }
}

void Compiler::EmitUnaryI64(LLOp op, uint16_t src_reg, uint16_t dest_reg) {
    switch (op) {
        case LL_NEG_I64:
            __ movq(xmm1, RegAddr(src_reg));
            __ pxor(xmm0, xmm0);
            __ psubq(xmm0, xmm1);
            __ movq(RegAddr(dest_reg), xmm0);
            break;
        case LL_INVERT_I64:
            __ movq(xmm0, RegAddr(src_reg));
            __ pcmpeqd(xmm1, xmm1);
            __ pxor(xmm0, xmm1);
            __ movq(RegAddr(dest_reg), xmm0);
            break;
        case LL_TEST_I64:
            __ movl(eax, RegAddr(src_reg));
            __ orl(eax, RegAddr(src_reg + 1));
            __ set(not_equal, eax);
            __ movzxb(eax, eax);
            __ movl(RegAddr(dest_reg), eax);
            break;
        case LL_CVT_I64:
            __ movl(eax, RegAddr(src_reg));
            __ movl(RegAddr(dest_reg), eax);
            __ sarl(eax, 0x1f);
            __ movl(RegAddr(dest_reg + 1), eax);
            break;
        case LL_TRUNCATE_I64:
            __ movl(eax, RegAddr(src_reg));
            __ movl(RegAddr(dest_reg), eax);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitSdivI64(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) {
    __ lea(ecx, RegAddr(dest));
    __ movl(Operand(esp, 8), ecx);
    __ lea(eax, RegAddr(lhs));
    __ movl(Operand(esp, 4), eax);
    __ lea(edx, RegAddr(rhs));
    __ movl(Operand(esp, 0), edx);
    if (op == LL_SDIV_I64)
        __ call(ExternalAddress(reinterpret_cast<void*>(Int64Div)));
    else if (op == LL_SMOD_I64)
        __ call(ExternalAddress(reinterpret_cast<void*>(Int64Mod)));
    else
        assert(false);
    __ testl(eax, eax);
    JumpAndReportOnError(not_zero);
}

void Compiler::EmitLoadInternedObj(uint32_t addr, uint16_t dest_reg) {
    __ movl(eax, addr);
    __ incl(Operand(eax, offsetof(HeapItem, rc)));
    __ movl(RegAddr(dest_reg), eax);
}

void Compiler::EmitLoadI(LLOp op, uint32_t src_reg, uint32_t dest_reg) {
    __ movl(eax, RegAddr(src_reg));
    switch (op) {
        case LL_LOAD_I_I32:
        case LL_LOAD_I_F32:
            __ movl(edx, Operand(eax, 0));
            __ movl(RegAddr(dest_reg), edx);
            break;
        case LL_LOAD_I_U8:
            __ movzxb(eax, Operand(eax, 0));
            __ movl(RegAddr(dest_reg), eax);
            break;
        case LL_LOAD_I_I64:
            __ movq(xmm0, Operand(eax, 0));
            __ movq(RegAddr(dest_reg), xmm0);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitStorI(LLOp op, uint32_t addr_reg, uint32_t val_reg) {
    __ movl(edx, RegAddr(addr_reg));
    switch (op) {
        case LL_STOR_I_I32:
        case LL_STOR_I_F32:
            __ movl(eax, RegAddr(val_reg));
            __ movl(Operand(edx, 0), eax);
            break;
        case LL_STOR_I_U8:
            __ movl(eax, RegAddr(val_reg));
            __ movb(Operand(edx, 0), eax);
            break;
        case LL_STOR_I_I64:
            __ movq(xmm0, RegAddr(val_reg));
            __ movq(Operand(edx, 0), xmm0);
            break;
        case LL_STOR_I_A:
            __ movl(eax, RegAddr(val_reg));
            EmitIncRefForArrayEscape(eax, ecx);
            __ movl(ecx, Operand(edx, 0));
            __ movl(Operand(edx, 0), eax);
            EmitDecRef(ecx, {});
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitLoadFld(LLOp op, uint16_t addr_reg, uint16_t offset, uint16_t dest_reg) {
    __ movl(edx, RegAddr(addr_reg));
    switch (op) {
        case LL_LOAD_FLD_X32:
            __ movl(eax, Operand(edx, offset));
            __ movl(RegAddr(dest_reg), eax);
            break;
        case LL_LOAD_FLD_X64:
            __ movq(xmm0, Operand(edx, offset));
            __ movq(RegAddr(dest_reg), xmm0);
            break;
        case LL_LOAD_FLD_A:
            __ movl(eax, Operand(edx, offset));
            EmitIncRef(eax);
            __ movl(RegAddr(dest_reg), eax);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitStorFld(LLOp op, uint16_t addr_reg, uint16_t offset, uint16_t val_reg) {
    __ movl(edx, RegAddr(addr_reg));
    switch (op) {
        case LL_STOR_FLD_X32:
            __ movl(eax, RegAddr(val_reg));
            __ movl(Operand(edx, offset), eax);
            break;
        case LL_STOR_FLD_X64:
            __ movq(xmm0, RegAddr(val_reg));
            __ movq(Operand(edx, offset), xmm0);
            break;
        case LL_STOR_FLD_A:
            __ movl(eax, RegAddr(val_reg));
            EmitIncRefForArrayEscape(eax, ecx);
            __ movl(ecx, Operand(edx, offset));
            __ movl(Operand(edx, offset), eax);
            EmitDecRef(ecx, {});
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

    auto src = ExternalAddress(reinterpret_cast<void*>(addr));
    switch (op) {
        case LL_LOAD_GLB_X32:
            __ movl(eax, Operand(src));
            __ movl(RegAddr(dest_reg), eax);
            break;
        case LL_LOAD_GLB_X64:
            __ movq(xmm0, Operand(src));
            __ movq(RegAddr(dest_reg), xmm0);
            break;
        case LL_LOAD_GLB_A:
            __ movl(eax, Operand(src));
            EmitIncRef(eax);
            __ movl(RegAddr(dest_reg), eax);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitStorGlb(LLOp op, uint32_t addr, uint16_t val_reg) {
    auto dest = ExternalAddress(reinterpret_cast<void*>(addr));
    switch (op) {
        case LL_STOR_GLB_X32:
            __ movl(eax, RegAddr(val_reg));
            __ movl(Operand(dest), eax);
            break;
        case LL_STOR_GLB_X64:
            __ movq(xmm0, RegAddr(val_reg));
            __ movq(Operand(dest), xmm0);
            break;
        case LL_STOR_GLB_A:
            __ movl(eax, RegAddr(val_reg));
            EmitIncRefForArrayEscape(eax, ecx);
            __ movl(ecx, Operand(dest));
            __ movl(Operand(dest), eax);
            EmitDecRef(ecx, {});
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitFillArray(uint16_t addr_reg, const void* data_addr, uint32_t data_size) {
    __ movl(Operand(esp, 0), edi);

    __ movl(edi, RegAddr(addr_reg));
    __ movl(edi, Operand(edi, offsetof(SpArray, data)));
    __ movl(esi, reinterpret_cast<int32_t>(data_addr));
    if (data_size >= 4) {
        __ movl(ecx, data_size / 4);
        __ rep_movsd();
    }
    if (data_size % 4) {
        __ movl(ecx, data_size % 4);
        __ rep_movsb();
    }

    __ movl(edi, Operand(esp, 0));
}

void Compiler::EmitFillArrayFlat(uint16_t addr_reg, const void* data_addr, uint32_t data_size) {
    __ movl(Operand(esp, 0), edi);

    __ movl(edi, RegAddr(addr_reg));
    __ movl(esi, reinterpret_cast<int32_t>(data_addr));
    if (data_size >= 4) {
        __ movl(ecx, data_size / 4);
        __ rep_movsd();
    }
    if (data_size % 4) {
        __ movl(ecx, data_size % 4);
        __ rep_movsb();
    }

    __ movl(edi, Operand(esp, 0));
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
    __ movl(eax, RegAddr(op.index_reg));
    __ cmpl(eax, op.size);

    auto& thunk = AddBoundsErrorThunk();
    thunk.index = eax;
    thunk.limit = op.size;
    __ j(above_equal, &thunk.label);

    auto scale = EltSizeToScale(op.elt_size);
    if (scale) {
        __ movl(ecx, RegAddr(op.base_reg));
        __ lea(edx, Operand(ecx, eax, *scale));
    } else {
        // :TODO: strength reduction?
        __ imull(edx, eax, op.elt_size);
        __ addl(edx, RegAddr(op.base_reg));
    }
    __ movl(RegAddr(op.dest_reg), edx);
}

void Compiler::EmitLoadElemFlat(LLOp op, const LoadElemFlatArgs& args) {
    __ movl(ecx, RegAddr(args.index_reg));
    __ cmpl(ecx, args.array_size);

    auto& thunk = AddBoundsErrorThunk();
    thunk.index = ecx;
    thunk.limit = args.array_size;
    __ j(above_equal, &thunk.label);

    int32_t base_offset = args.base_reg * sizeof(cell_t);
    switch (op) {
        case LL_LOAD_ELEM_FLAT_I32:
        case LL_LOAD_ELEM_FLAT_F32:
            __ movl(eax, Operand(frm, ecx, ScaleFour, base_offset));
            __ movl(RegAddr(args.dest_reg), eax);
            break;
        case LL_LOAD_ELEM_FLAT_U8:
            __ movzxb(eax, Operand(frm, ecx, NoScale, base_offset));
            __ movl(RegAddr(args.dest_reg), eax);
            break;
        case LL_LOAD_ELEM_FLAT_I64:
            __ movq(xmm0, Operand(frm, ecx, ScaleEight, base_offset));
            __ movq(RegAddr(args.dest_reg), xmm0);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitLoadElemFlatI(LLOp op, const LoadElemFlatArgs& args) {
    __ movl(ecx, RegAddr(args.index_reg));
    __ cmpl(ecx, args.array_size);

    auto& thunk = AddBoundsErrorThunk();
    thunk.index = ecx;
    thunk.limit = args.array_size;
    __ j(above_equal, &thunk.label);

    __ movl(edx, RegAddr(args.base_reg));
    switch (op) {
        case LL_LOAD_ELEM_FLAT_I_I32:
        case LL_LOAD_ELEM_FLAT_I_F32:
            __ movl(eax, Operand(edx, ecx, ScaleFour));
            __ movl(RegAddr(args.dest_reg), eax);
            break;
        case LL_LOAD_ELEM_FLAT_I_U8:
            __ movzxb(eax, Operand(edx, ecx, NoScale));
            __ movl(RegAddr(args.dest_reg), eax);
            break;
        case LL_LOAD_ELEM_FLAT_I_I64:
            __ movq(xmm0, Operand(edx, ecx, ScaleEight));
            __ movq(RegAddr(args.dest_reg), xmm0);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitStorElemFlat(LLOp op, const StorElemFlatArgs& args) {
    __ movl(ecx, RegAddr(args.index_reg));
    __ cmpl(ecx, args.array_size);

    auto& thunk = AddBoundsErrorThunk();
    thunk.index = ecx;
    thunk.limit = args.array_size;
    __ j(above_equal, &thunk.label);

    int32_t base_offset = args.base_reg * sizeof(cell_t);
    switch (op) {
        case LL_STOR_ELEM_FLAT_I32:
        case LL_STOR_ELEM_FLAT_F32:
            __ movl(eax, RegAddr(args.val_reg));
            __ movl(Operand(frm, ecx, ScaleFour, base_offset), eax);
            break;
        case LL_STOR_ELEM_FLAT_U8:
            __ movl(eax, RegAddr(args.val_reg));
            __ movb(Operand(frm, ecx, NoScale, base_offset), eax);
            break;
        case LL_STOR_ELEM_FLAT_I64:
            __ movq(xmm0, RegAddr(args.val_reg));
            __ movq(Operand(frm, ecx, ScaleEight, base_offset), xmm0);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitStorElemFlatI(LLOp op, const StorElemFlatArgs& args) {
    __ movl(ecx, RegAddr(args.index_reg));
    __ cmpl(ecx, args.array_size);

    auto& thunk = AddBoundsErrorThunk();
    thunk.index = ecx;
    thunk.limit = args.array_size;
    __ j(above_equal, &thunk.label);

    __ movl(edx, RegAddr(args.base_reg));
    switch (op) {
        case LL_STOR_ELEM_FLAT_I_I32:
        case LL_STOR_ELEM_FLAT_I_F32:
            __ movl(eax, RegAddr(args.val_reg));
            __ movl(Operand(edx, ecx, ScaleFour), eax);
            break;
        case LL_STOR_ELEM_FLAT_I_U8:
            __ movl(eax, RegAddr(args.val_reg));
            __ movb(Operand(edx, ecx, NoScale), eax);
            break;
        case LL_STOR_ELEM_FLAT_I_I64:
            __ movq(xmm0, RegAddr(args.val_reg));
            __ movq(Operand(edx, ecx, ScaleEight), xmm0);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitLoadElem(LLOp op, uint16_t base_reg, uint16_t index_reg, uint16_t dest_reg) {
    __ movl(edx, RegAddr(base_reg));
    __ testl(edx, edx);
    JumpOnError(zero, SP_ERROR_NULL_DEREF);

    __ movl(ecx, RegAddr(index_reg));
    __ movl(eax, Operand(edx, offsetof(SpArray, length)));
    __ cmpl(ecx, eax);

    auto& thunk = AddBoundsErrorThunk();
    thunk.index = ecx;
    thunk.limit = eax;
    __ j(above_equal, &thunk.label);

    __ movl(edx, Operand(edx, offsetof(SpArray, data)));
    switch (op) {
        case LL_LOAD_ELEM_I32:
        case LL_LOAD_ELEM_F32:
            __ movl(eax, Operand(edx, ecx, ScaleFour));
            __ movl(RegAddr(dest_reg), eax);
            break;
        case LL_LOAD_ELEM_U8:
            __ movzxb(eax, Operand(edx, ecx, NoScale));
            __ movl(RegAddr(dest_reg), eax);
            break;
        case LL_LOAD_ELEM_I64:
            __ movq(xmm0, Operand(edx, ecx, ScaleEight));
            __ movq(RegAddr(dest_reg), xmm0);
            break;
        case LL_LOAD_ELEM_A:
            __ movl(eax, Operand(edx, ecx, ScaleFour));
            EmitIncRef(eax);
            __ movl(RegAddr(dest_reg), eax);
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitStorElem(LLOp op, uint16_t base_reg, uint16_t index_reg, uint16_t val_reg) {
    __ movl(edx, RegAddr(base_reg));
    __ testl(edx, edx);
    JumpOnError(zero, SP_ERROR_NULL_DEREF);

    __ movl(ecx, RegAddr(index_reg));
    __ movl(eax, Operand(edx, offsetof(SpArray, length)));
    __ cmpl(ecx, eax);

    auto& thunk = AddBoundsErrorThunk();
    thunk.index = ecx;
    thunk.limit = eax;
    __ j(above_equal, &thunk.label);

    __ movl(edx, Operand(edx, offsetof(SpArray, data)));
    switch (op) {
        case LL_STOR_ELEM_I32:
        case LL_STOR_ELEM_F32:
            __ movl(eax, RegAddr(val_reg));
            __ movl(Operand(edx, ecx, ScaleFour), eax);
            break;
        case LL_STOR_ELEM_U8:
            __ movl(eax, RegAddr(val_reg));
            __ movb(Operand(edx, ecx, NoScale), eax);
            break;
        case LL_STOR_ELEM_I64:
            __ movq(xmm0, RegAddr(val_reg));
            __ movq(Operand(edx, ecx, ScaleEight), xmm0);
            break;
        case LL_STOR_ELEM_A:
            __ movl(eax, RegAddr(val_reg));
            EmitIncRefForArrayEscape(eax, esi);
            __ lea(edx, Operand(edx, ecx, ScaleFour));
            __ movl(ecx, Operand(edx, 0));
            __ movl(Operand(edx, 0), eax);
            EmitDecRef(ecx, {});
            break;
        default:
            assert(false);
    }
}

void Compiler::EmitBoundsErrorThunk(BoundsErrorThunk* thunk) {
    if (std::holds_alternative<Register>(thunk->limit))
        __ movl(Operand(esp, 4), std::get<Register>(thunk->limit));
    else
        __ movl(Operand(esp, 4), std::get<uint32_t>(thunk->limit));

    if (std::holds_alternative<Register>(thunk->index))
        __ movl(Operand(esp, 0), std::get<Register>(thunk->index));
    else
        __ movl(Operand(esp, 0), std::get<uint32_t>(thunk->index));

    __ call(ExternalAddress(env_->stubs()->return_stubs_v2().bounds_error));
    EmitCipMapping(thunk->cip);
}

void Compiler::EmitDeferredErrorThunk(DeferredErrorThunk* thunk) {
    __ call(ExternalAddress(stubs_.deferred_error));
    EmitCipMapping(thunk->cip);
}

void Compiler::CallRtForHandle(void* method_addr, uint16_t dest_reg) {
#if defined(_WIN32)
    // Handle<> pointer.
    __ lea(edx, Operand(ebp, kHandleOffset));
    __ push(edx);
    // |this|
    __ movl(ecx, reinterpret_cast<intptr_t>(rt_));
#else
    // Handle<> pointer.
    __ lea(eax, Operand(ebp, kHandleOffset));
    __ movl(Operand(esp, 0), eax);
    // |this|
    __ movl(Operand(esp, 4), reinterpret_cast<intptr_t>(rt_));
#endif

    // Clear exit_fp_ so DispatchReport defers the error.
    __ movl(Operand(ExternalAddress(env_->addressOfExit())), 0);

    __ call(ExternalAddress(method_addr));

#if !defined(_WIN32)
    // SysV: callee pops hidden return pointer.
    __ subl(esp, 4);
#endif

    // Extract the handle value.
    __ movl(eax, Operand(ebp, kHandleOffset));
    __ testl(eax, eax);

    auto& thunk = AddDeferredErrorThunk();
    __ j(zero, &thunk.label);

    __ movl(RegAddr(dest_reg), eax);
}

void Compiler::EmitSlice(uint16_t base_reg, uint16_t index_reg, uint16_t dest_reg) {
    __ movl(ecx, RegAddr(base_reg));
    __ testl(ecx, ecx);
    JumpOnError(zero, SP_ERROR_NULL_DEREF);

#if defined(_WIN32)
    __ push(RegAddr(index_reg));
    __ push(ecx);
#else
    __ movl(eax, RegAddr(index_reg));
    __ movl(Operand(esp, 12), eax);
    __ movl(Operand(esp, 8), ecx);
#endif
    CallRtForHandle(PmfCast<void*>(&Runtime::NewSlice), dest_reg);
}

void Compiler::EmitSliceEs(uint16_t src_reg, uint16_t dest_reg, uint32_t cells) {
#if defined(_WIN32)
    __ push(cells);
    __ push(RegAddr(src_reg));
#else
    __ movl(Operand(esp, 12), cells);
    __ movl(ecx, RegAddr(src_reg));
    __ movl(Operand(esp, 8), ecx);
#endif
    CallRtForHandle(PmfCast<void*>(&Runtime::NewSliceEs), dest_reg);
}

void Compiler::EmitSliceFlat(const SliceFlatArgs& op) {
#if defined(_WIN32)
    __ push(RegAddr(op.index_reg));
    __ push(reinterpret_cast<intptr_t>(op.td));
    __ push(RegAddr(op.base_reg));
#else
    __ movl(eax, RegAddr(op.index_reg));
    __ movl(Operand(esp, 16), eax);
    __ movl(Operand(esp, 12), reinterpret_cast<intptr_t>(op.td));
    __ movl(eax, RegAddr(op.base_reg));
    __ movl(Operand(esp, 8), eax);
#endif
    CallRtForHandle(PmfCast<void*>(&Runtime::NewFlatSlice), op.dest_reg);
}

void Compiler::EmitIdxAddr(const IdxAddrArgs& op) {
    __ movl(eax, RegAddr(op.base_reg));
    __ testl(eax, eax);
    JumpOnError(zero, SP_ERROR_NULL_DEREF);

    __ movl(ecx, RegAddr(op.index_reg));
    __ movl(edx, Operand(eax, offsetof(SpArray, length)));

    auto& thunk = AddBoundsErrorThunk();
    thunk.index = ecx;
    thunk.limit = edx;
    __ cmpl(ecx, edx);
    __ j(above_equal, &thunk.label);

    __ movl(eax, Operand(eax, offsetof(SpArray, data)));
    if (auto scale = EltSizeToScale(op.elt_size)) {
        __ lea(eax, Operand(eax, ecx, *scale));
    } else {
        __ imull(edx, ecx, op.elt_size);
        __ addl(eax, edx);
    }
    __ movl(RegAddr(op.dest_reg), eax);
}

void Compiler::EmitCopyArray(LLOp op, uint16_t src_reg, uint16_t dest_reg, uint32_t bytes) {
    __ movl(Operand(esp, 0), edi);

    __ movl(esi, RegAddr(src_reg));
    __ movl(edi, RegAddr(dest_reg));

    if (op == LL_COPYARRAY) {
        __ testl(esi, esi);
        JumpOnError(zero, SP_ERROR_NULL_DEREF);
        __ movl(esi, Operand(esi, offsetof(SpArray, data)));

        __ testl(edi, edi);
        JumpOnError(zero, SP_ERROR_NULL_DEREF);
        __ movl(edi, Operand(edi, offsetof(SpArray, data)));
    }

    if (bytes >= 4) {
        __ movl(ecx, bytes / 4);
        __ rep_movsd();
    }
    if (bytes % 4) {
        __ movl(ecx, bytes % 4);
        __ rep_movsb();
    }

    __ movl(edi, Operand(esp, 0));
}

void Compiler::EmitArrayToFlat(uint16_t src_reg, uint16_t dest_reg) {
    __ movl(edx, RegAddr(src_reg));
    __ testl(edx, edx);
    JumpOnError(zero, SP_ERROR_NULL_DEREF);

    __ movl(eax, Operand(edx, offsetof(SpArray, data)));
    __ movl(RegAddr(dest_reg), eax);
}

void Compiler::EmitAddrFld(uint16_t src_reg, uint16_t dest_reg, uint32_t offset) {
    __ movl(eax, RegAddr(src_reg));
    __ lea(eax, Operand(eax, offset));
    __ movl(RegAddr(dest_reg), eax);
}

void Compiler::EmitSwitchChain(uint16_t val_reg, uint32_t def_block,
                               const std::span<const SwitchCaseEntry>& cases)
{
    __ movl(eax, RegAddr(val_reg));
    for (size_t i = 0; i < cases.size(); i++) {
        const auto& entry = cases[i];

        __ cmpl(eax, entry.value);
        __ j(equal, &block_labels_[block_->successors[i + 1]]);
    }
    __ jmp(&block_labels_[def_block]);
}

void Compiler::EmitSwitchTable(uint16_t val_reg, uint32_t def_block,
                               const std::span<const SwitchCaseEntry>& cases)
{
    __ movl(ecx, RegAddr(val_reg));

    cell_t low = cases[0].value;
    if (low != 0) {
        low = -low;
        __ lea(ecx, Operand(ecx, low));
    }

    cell_t high = abs(cases[0].value - cases.back().value);
    __ cmpl(ecx, high);
    __ j(above, &block_labels_[def_block]);

    CodeLabel table;
    __ movl(edx, &table);
    __ movl(eax, Operand(edx, ecx, ScaleFour));
    __ jmp(eax);

    __ bind(&table);
    for (size_t i = 0; i < cases.size(); i++) {
        uint32_t target = block_->successors[i + 1];
        __ emit_absolute_address(&block_labels_[target]);
    }
}

void Compiler::EmitCopyObj(uint16_t src_reg, uint16_t dest_reg, uint32_t bytes) {
    assert(bytes % 4 == 0);

    __ movl(Operand(esp, 0), edi);
    __ movl(edi, RegAddr(dest_reg));
    __ movl(esi, RegAddr(src_reg));
    __ movl(ecx, bytes / 4);
    __ rep_movsd();
    __ movl(edi, Operand(esp, 0));
}

void Compiler::EmitDeallocThunk(DeallocThunk* thunk) {
    if (thunk->save_reg)
        __ movl(Operand(esp, 4), *thunk->save_reg);

    __ movl(Operand(esp, 0), thunk->obj_reg);
    __ callWithABI(ExternalAddress(env_->stubs()->DeallocStub()));
    EmitCipMapping(thunk->cip);

    if (thunk->save_reg)
        __ movl(*thunk->save_reg, Operand(esp, 4));

    __ jmp(&thunk->return_label);
}

} // namespace sp::v2
