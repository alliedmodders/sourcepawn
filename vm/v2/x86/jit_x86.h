// vim: set ts=8 sts=2 sw=2 tw=99 et:
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
#pragma once

#include <amtl/am-vector.h>
#include <sp_vm_api.h>
#include <sp_vm_types.h>
#include "compiled-function.h"
#include "constants-x86.h"
#include "v2/jit.h"
#include "macro-assembler.h"
#include "v2/opcodes.h"
#include "v2/runtime.h"

using namespace SourcePawn;

namespace sp {
class CompiledFunction;
class Environment;
class SmxImage;
} // namespace sp

namespace sp::v2 {
class CallThunk;

class Compiler : public CompilerBase
{
  public:
    Compiler(Runtime* rt, MethodInfo* method);
    ~Compiler();

    void EmitLoadConst(uint16_t reg, cell_t val) override;
    void EmitLoadConst64(uint16_t reg, int64_t val) override;
    void EmitAddr(uint16_t src_reg, uint16_t dest_reg) override;
    void EmitRetn(LLOp op, std::optional<uint16_t> reg) override;
    void EmitNativeCall(uint32_t native_index, uint8_t nargs, uint16_t dest, const std::vector<uint16_t>& args) override;
    void EmitScriptedCall(uint32_t method_index, uint8_t nargs, uint16_t dest, const std::vector<uint16_t>& args) override;
    void EmitJump(size_t target_idx) override;
    void EmitJump(LLOp op, uint16_t src_reg, size_t target_idx) override;
    void EmitJumpCmp(LLOp op, uint16_t reg_a, uint16_t reg_b, size_t target_idx) override;
    void EmitCmpI32(LLOp op, uint16_t reg_a, uint16_t reg_b, uint16_t dest) override;
    void EmitBasicAlu(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) override;
    void EmitUnaryAlu(LLOp op, uint16_t src_reg, uint16_t dest_reg) override;
    void EmitSdivI32(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) override;
    void EmitCompareFloat(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) override;
    void EmitBinaryFloatOp(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) override;
    void EmitUnaryFloatOp(LLOp op, uint16_t src_reg, uint16_t dest_reg) override;
    void EmitMove(LLOp op, uint16_t src_reg, uint16_t dest_reg) override;
    void EmitNewArray(const TypeDesc* td, uint16_t size_reg, uint16_t dest_reg) override;
    void EmitNewFixedArray(const TypeDesc* td, uint16_t dest_reg, uint32_t size) override;
    void EmitNewBulkArray(uint8_t dims, const TypeDesc* td, uint16_t size_reg,
                          uint16_t dest_reg) override;
    void EmitAddRef(uint16_t reg) override;
    void EmitRelease(uint16_t reg) override;
    void EmitCmpI64(LLOp op, uint16_t reg_a, uint16_t reg_b, uint16_t dest) override;
    void EmitBinaryI64(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) override;
    void EmitUnaryI64(LLOp op, uint16_t src_reg, uint16_t dest_reg) override;
    void EmitSdivI64(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) override;
    void EmitLoadInternedObj(uint32_t addr, uint16_t dest_reg) override;
    void EmitArrayToNative(uint32_t src_reg, uint32_t dest_reg) override;
    void EmitLoadI(LLOp op, uint32_t src_reg, uint32_t dest_reg) override;
    void EmitStorI(LLOp op, uint32_t addr_reg, uint32_t val_reg) override;
    void EmitLoadFld(LLOp op, uint16_t addr_reg, uint16_t offset, uint16_t dest_reg) override;
    void EmitStorFld(LLOp op, uint16_t addr_reg, uint16_t offset, uint16_t val_reg) override;
    void EmitLoadGlb(LLOp op, uint32_t addr, uint16_t dest_reg) override;
    void EmitStorGlb(LLOp op, uint32_t addr, uint16_t val_reg) override;
    void EmitFillArray(uint16_t addr_reg, const void* data_addr, uint32_t data_size) override;
    void EmitFillArrayFlat(uint16_t addr_reg, const void* data_addr, uint32_t data_size) override;
    void EmitIdxAddrFlat(const IdxAddrFlatArgs& op) override;
    void EmitLoadElemFlat(LLOp op, const LoadElemFlatArgs& args) override;
    void EmitStorElemFlat(LLOp op, const StorElemFlatArgs& args) override;
    void EmitLoadElem(LLOp op, uint16_t base_reg, uint16_t index_reg, uint16_t dest_reg) override;
    void EmitStorElem(LLOp op, uint16_t base_reg, uint16_t index_reg, uint16_t val_reg) override;
    void EmitSlice(uint16_t base_reg, uint16_t index_reg, uint16_t dest_reg) override;
    void EmitSliceEs(uint16_t src_reg, uint16_t dest_reg, uint32_t cells) override;
    void EmitSliceFlat(const SliceFlatArgs& op) override;
    void EmitIdxAddr(const IdxAddrArgs& op) override;
    void EmitCopyArray(LLOp op, uint16_t src_reg, uint16_t dest_reg, uint32_t bytes) override;
    void EmitCopyObj(uint16_t src_reg, uint16_t dest_reg, uint32_t bytes) override;
    void EmitArrayToFlat(uint16_t src_reg, uint16_t dest_reg) override;
    void EmitAddrFld(uint16_t src_reg, uint16_t dest_reg, uint32_t offset) override;
    void EmitSwitchChain(uint16_t val_reg, uint32_t def_block,
                         const std::span<const SwitchCaseEntry>& cases) override;
    void EmitSwitchTable(uint16_t val_reg, uint32_t def_block,
                         const std::span<const SwitchCaseEntry>& cases) override;

    void EmitDeallocThunk(DeallocThunk* thunk) override;
    void EmitBoundsErrorThunk(BoundsErrorThunk* thunk) override;
    void EmitDeferredErrorThunk(DeferredErrorThunk* thunk) override;

  private:
    void EmitIncRef(Register obj_reg);
    void EmitIncRefForArrayEscape(Register obj_reg, Register tmp);
    void EmitDecRef(Register obj_reg, std::optional<Register> save_reg,
                    const std::optional<Operand>& zero_loc = {});

    ExternalAddress spAddr() { return ExternalAddress(env_->addressOfSp()); }
    ExternalAddress spBaseAddr() { return ExternalAddress(env_->addressOfSpBase()); }

    Operand RegAddr(uint32_t reg) {
        return Operand(frm, int32_t(reg * sizeof(cell_t)));
    }

    // Emits a call to a Handle<>-returning Runtime method with deferred error
    // reporting. The caller pre-places method-specific arguments on the stack.
    void CallRtForHandle(void* method_addr, uint16_t dest_reg);
};

} // namespace sp::v2
