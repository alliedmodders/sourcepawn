// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

#include <assert.h>

#include <optional>
#include <span>
#include <vector>

#include "utils/cxx_helpers.h"
#include "v2/jit.h"
#include "v2/runtime.h"
#include "v2/x64/constants-x64.h"

namespace sp {
class CompiledFunction;
class SmxImage;
}

namespace sp::v2 {

using namespace SourcePawn;

class CallThunk;

class Compiler : public CompilerBase
{
  public:
    Compiler(Runtime* rt, MethodInfo* method);
    ~Compiler();

    void EmitPrologue(const FrameInfo& frame) override;
    void EmitLoadConst(uint16_t reg, cell_t val) override;
    void EmitLoadConst64(uint16_t reg, int64_t val) override;
    void EmitAddr(uint16_t src_reg, uint16_t dest_reg) override;
    void EmitRetn(LLOp op, std::optional<uint16_t> reg) override;
    void EmitEpilogue() override;
    void EmitNativeCall(uint32_t native_index, uint8_t nargs, uint16_t dest,
                        const std::vector<uint16_t>& args, uint16_t spread_reg) override;
    void EmitScriptedCall(uint32_t method_index, uint8_t nargs, uint16_t dest,
                          const std::vector<uint16_t>& args) override;
    void EmitIndirectCall(uint32_t fn_reg, uint8_t nargs, uint16_t dest,
                          const std::vector<uint16_t>& args) override;
    void EmitGetFuncId(uint16_t src_reg, uint16_t dest_reg) override;
    void EmitGetFnObj(uint16_t src_reg, const TypeDesc* td, uint16_t dest_reg) override;
    void EmitJump(size_t target_idx) override;
    void EmitJump(LLOp op, uint16_t src_reg, size_t target_idx) override;
    void EmitJumpCmp(LLOp op, uint16_t reg_a, uint16_t reg_b, size_t target_idx) override;
    void EmitCmpI32(LLOp op, uint16_t reg_a, uint16_t reg_b, uint16_t dest) override;
    void EmitBasicAlu(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) override;
    void EmitUnaryAlu(LLOp op, uint16_t src_reg, uint16_t dest_reg) override;
    void EmitSdivI32(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) override;
    void EmitCompareFloat(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest,
                          sp::TypeKind kind) override;
    void EmitBinaryFloatOp(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) override;
    void EmitUnaryFloatOp(LLOp op, uint16_t src_reg, uint16_t dest_reg) override;
    void EmitMove(LLOp op, uint16_t src_reg, uint16_t dest_reg) override;
    void EmitNewArray(const TypeDesc* td, uint16_t size_reg, uint16_t dest_reg) override;
    void EmitNewFixedArray(const TypeDesc* td, uint16_t dest_reg, uint32_t size) override;
    void EmitNewBulkArray(uint8_t dims, const TypeDesc* td, uint16_t size_reg, uint16_t dest_reg) override;
    void EmitNewObj(const TypeDesc* td, uint16_t dest_reg) override;
    void EmitNewClosure(const TypeDesc* closure_td, MethodInfo* method, uint16_t dest_reg) override;
    void EmitAddRef(uint16_t reg) override;
    void EmitRelease(uint16_t reg) override;
    void EmitCmpI64(LLOp op, uint16_t reg_a, uint16_t reg_b, uint16_t dest) override;
    void EmitBinaryI64(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) override;
    void EmitUnaryI64(LLOp op, uint16_t src_reg, uint16_t dest_reg) override;
    void EmitSdivI64(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) override;
    void EmitBinaryDoubleOp(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) override;
    void EmitUnaryDoubleOp(LLOp op, uint16_t src_reg, uint16_t dest_reg) override;
    void EmitLoadInternedObj(uint32_t addr, uint16_t dest_reg) override;
    void EmitLoadI(LLOp op, uint32_t src_reg, uint32_t dest_reg) override;
    void EmitStorI(LLOp op, uint32_t addr_reg, uint32_t val_reg) override;
    void EmitLoadFld(LLOp op, uint16_t addr_reg, uint16_t offset, uint16_t dest_reg) override;
    void EmitStorFld(LLOp op, uint16_t addr_reg, uint16_t offset, uint16_t val_reg) override;
    void EmitLoadGlb(LLOp op, uint32_t addr, uint16_t dest_reg) override;
    void EmitStorGlb(LLOp op, uint32_t addr, uint16_t val_reg) override;
    void EmitFillArray(uint16_t addr_reg, const void* data, uint32_t data_size,
                       uint32_t pad_bytes) override;
    void EmitFillArrayFlat(uint16_t addr_reg, const void* data_addr, uint32_t data_size,
                           uint32_t pad_bytes) override;
    void EmitIdxAddrFlat(const IdxAddrFlatArgs& op) override;
    void EmitLoadElemFlat(LLOp op, const LoadElemFlatArgs& args) override;
    void EmitLoadElemFlatI(LLOp op, const LoadElemFlatArgs& args) override;
    void EmitStorElemFlat(LLOp op, const StorElemFlatArgs& args) override;
    void EmitStorElemFlatI(LLOp op, const StorElemFlatArgs& args) override;
    void EmitLoadElem(LLOp op, uint16_t base_reg, uint16_t index_reg, uint16_t dest_reg) override;
    void EmitStorElem(LLOp op, uint16_t base_reg, uint16_t index_reg, uint16_t val_reg) override;
    void EmitLoadUpvar(LLOp op, const UpvarArgs& args) override;
    void EmitStorUpvar(LLOp op, const UpvarArgs& args) override;
    void EmitSlice(uint16_t base_reg, uint16_t index_reg, uint16_t dest_reg) override;
    void EmitSliceEs(uint16_t src_reg, uint16_t dest_reg, uint32_t cells) override;
    void EmitSliceFlat(const SliceFlatArgs& op) override;
    void EmitIdxAddr(const IdxAddrArgs& args) override;
    void EmitCopyArray(LLOp op, uint16_t src_reg, uint16_t dest_reg, uint32_t bytes) override;
    void EmitCopyArrayFlatA(uint16_t src_reg, uint16_t dest_reg, uint32_t count) override;
    void EmitCopyArrayA(uint16_t src_reg, uint16_t dest_reg) override;
    void EmitCopyObj(uint16_t src_reg, uint16_t dest_reg, uint32_t bytes) override;
    void EmitArrayToFlat(uint16_t src_reg, uint16_t dest_reg) override;
    void EmitAddrFld(uint16_t src_reg, uint16_t dest_reg, uint32_t offset) override;
    void EmitSwitchChain(uint16_t val_reg, uint32_t def_block, const std::span<const SwitchCaseEntry>& cases) override;
    void EmitSwitchTable(uint16_t val_reg, uint32_t def_block, const std::span<const SwitchCaseEntry>& cases) override;
    void EmitCallee(uint16_t dest_reg) override;
    void EmitSizeofArray(uint16_t src_reg, uint16_t dest_reg) override;

    void EmitDeallocThunk(DeallocThunk* thunk) override;
    void EmitBoundsErrorThunk(BoundsErrorThunk* thunk) override;
    void EmitDeferredErrorThunk(DeferredErrorThunk* thunk) override;
    void EmitCallThunk(CallThunk* thunk) override;
    void EmitIndirectCallThunk(IndirectCallThunk* thunk) override;

  private:
    void EmitIncRefForArrayEscape(Register obj_reg, Register tmp_reg);
    void EmitIncRef(Register obj_reg);
    void EmitDecRef(Register obj_reg, std::optional<Register> save_reg,
                    const std::optional<Operand>& zero_loc = {});
    void CallRtForHandleImpl(void* method_addr, uint32_t nargs, uint16_t dest_reg);
    void CallRtForBoolImpl(void* method_addr, uint32_t nargs);

    void JumpOnError(ConditionCode cc, int err);
    void JumpOnReportedError(ConditionCode cc);

    template <typename T>
    void CallRtForHandle(T method, uint32_t nargs, uint16_t dest_reg) {
        assert(sp::MemberFunctionArgCount<T>::value == nargs);
        CallRtForHandleImpl(ke::PmfCast<void*>(method), nargs, dest_reg);
    }

    template <typename T>
    void CallRtForBool(T method, uint32_t nargs) {
        assert(sp::MemberFunctionArgCount<T>::value == nargs);
        CallRtForBoolImpl(ke::PmfCast<void*>(method), nargs);
    }

    Operand RegAddr(uint32_t reg) {
        return Operand(frm, int32_t(reg * sizeof(cell_t)));
    }
    Operand StkRelAddr(uint32_t reg);
    Operand HeapAddr(Register addr_reg, uint32_t offset = 0) {
        return Operand(dat_reg, addr_reg, NoScale, offset);
    }
};

} // namespace sp::v2
