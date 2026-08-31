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
#pragma once

#include <optional>
#include <variant>
#include <vector>

#include <amtl/am-vector.h>
#include <sp_vm_api.h>
#include <sp_vm_types.h>
#include "code-stubs.h"
#include "compiled-function.h"
#include "macro-assembler.h"
#include "type-desc.h"
#include "v2/control-flow.h"
#include "v2/lowering/ll-op.h"
#include "v2/opcodes.h"

namespace sp {
class CompiledFunction;
class SmxImage;
} // namespace sp

namespace sp::v2 {

class LLCode;
class Runtime;
struct LLBlock;

struct BackwardJump {
    // The pc at the jump instruction (i.e. after it).
    uint32_t pc;
    // The cip of the jump.
    const uint8_t* cip;
    // The offset of the timeout thunk. This is filled in at the end.
    uint32_t timeout_offset;

    BackwardJump() {}
    BackwardJump(uint32_t pc, const uint8_t* cip)
     : pc(pc),
       cip(cip)
    {}
};

class CompilerBase
{
    friend class ErrorPath;

  public:
    CompilerBase(Runtime* rt, MethodInfo* method);
    virtual ~CompilerBase();

    static bool Compile(Runtime* cx, RefPtr<MethodInfo> method);

    static bool IsSupported();
    static bool SupportsPlugin(Runtime* cx);

  protected:
    CompiledFunction* Emit();
    bool CompileBlock(const LLBlock& block);

    struct FrameInfo {
        uint32_t num_regs;
        uint32_t frame_size;
        uint32_t num_params;
        uint32_t callee_regs;
    };

    virtual void EmitPrologue(const FrameInfo& frame) = 0;
    virtual void EmitLoadConst(uint16_t reg, cell_t val) = 0;
    virtual void EmitLoadConst64(uint16_t reg, int64_t val) = 0;
    virtual void EmitAddr(uint16_t src_reg, uint16_t dest_reg) = 0;
    virtual void EmitRetn(LLOp op, std::optional<uint16_t> reg) = 0;
    virtual void EmitNativeCall(uint32_t native_index, uint8_t nargs, uint16_t dest,
                                const std::vector<uint16_t>& args, uint16_t spread_reg) = 0;
    virtual void EmitScriptedCall(uint32_t method_index, uint8_t nargs, uint16_t dest,
                                  const std::vector<uint16_t>& args) = 0;
    virtual void EmitIndirectCall(uint32_t fn_reg, uint8_t nargs, uint16_t dest,
                                  const std::vector<uint16_t>& args) = 0;
    virtual void EmitGetFuncId(uint16_t src_reg, uint16_t dest_reg) = 0;
    virtual void EmitJump(size_t target_idx) = 0;
    virtual void EmitJump(LLOp op, uint16_t src_reg, size_t target_idx) = 0;
    virtual void EmitJumpCmp(LLOp op, uint16_t reg_a, uint16_t reg_b, size_t target_idx) = 0;
    virtual void EmitCmpI32(LLOp op, uint16_t reg_a, uint16_t reg_b, uint16_t dest) = 0;
    virtual void EmitBasicAlu(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) = 0;
    virtual void EmitUnaryAlu(LLOp op, uint16_t src_reg, uint16_t dest_reg) = 0;
    virtual void EmitSdivI32(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) = 0;
    virtual void EmitCompareFloat(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest,
                                  TypeKind kind) = 0;
    virtual void EmitBinaryFloatOp(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) = 0;
    virtual void EmitUnaryFloatOp(LLOp op, uint16_t src_reg, uint16_t dest_reg) = 0;
    virtual void EmitMove(LLOp op, uint16_t src_reg, uint16_t dest_reg) = 0;
    virtual void EmitNewArray(const TypeDesc* td, uint16_t size_reg, uint16_t dest_reg) = 0;
    virtual void EmitNewFixedArray(const TypeDesc* td, uint16_t dest_reg, uint32_t size) = 0;
    virtual void EmitNewBulkArray(uint8_t dims, const TypeDesc* td, uint16_t size_reg,
                                  uint16_t dest_reg) = 0;
    virtual void EmitNewObj(const TypeDesc* td, uint16_t dest_reg) = 0;
    virtual void EmitNewClosure(const TypeDesc* closure_td, MethodInfo* method,
                                uint16_t dest_reg) = 0;
    virtual void EmitAddRef(uint16_t reg) = 0;
    virtual void EmitRelease(uint16_t reg) = 0;
    virtual void EmitCmpI64(LLOp op, uint16_t reg_a, uint16_t reg_b, uint16_t dest) = 0;
    virtual void EmitBinaryI64(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) = 0;
    virtual void EmitUnaryI64(LLOp op, uint16_t src_reg, uint16_t dest_reg) = 0;
    virtual void EmitSdivI64(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) = 0;
    virtual void EmitBinaryDoubleOp(LLOp op, uint16_t lhs, uint16_t rhs, uint16_t dest) = 0;
    virtual void EmitUnaryDoubleOp(LLOp op, uint16_t src_reg, uint16_t dest_reg) = 0;
    virtual void EmitLoadInternedObj(uint32_t addr, uint16_t dest_reg) = 0;
    virtual void EmitLoadI(LLOp op, uint32_t src_reg, uint32_t dest_reg) = 0;
    virtual void EmitStorI(LLOp op, uint32_t addr_reg, uint32_t val_reg) = 0;
    virtual void EmitLoadFld(LLOp op, uint16_t addr_reg, uint16_t offset, uint16_t dest_reg) = 0;
    virtual void EmitStorFld(LLOp op, uint16_t addr_reg, uint16_t offset, uint16_t val_reg) = 0;
    virtual void EmitLoadGlb(LLOp op, uint32_t addr, uint16_t dest_reg) = 0;
    virtual void EmitStorGlb(LLOp op, uint32_t addr, uint16_t val_reg) = 0;
    virtual void EmitFillArray(uint16_t addr_reg, const void* data, uint32_t data_size) = 0;
    virtual void EmitFillArrayFlat(uint16_t addr_reg, const void* data_addr, uint32_t data_size) = 0;
    virtual void EmitIdxAddrFlat(const IdxAddrFlatArgs& op) = 0;
    virtual void EmitLoadElemFlat(LLOp op, const LoadElemFlatArgs& args) = 0;
    virtual void EmitLoadElemFlatI(LLOp op, const LoadElemFlatArgs& args) = 0;
    virtual void EmitStorElemFlat(LLOp op, const StorElemFlatArgs& args) = 0;
    virtual void EmitStorElemFlatI(LLOp op, const StorElemFlatArgs& args) = 0;
    virtual void EmitLoadElem(LLOp op, uint16_t base_reg, uint16_t index_reg, uint16_t dest_reg) = 0;
    virtual void EmitStorElem(LLOp op, uint16_t base_reg, uint16_t index_reg, uint16_t val_reg) = 0;
    virtual void EmitLoadUpvar(LLOp op, const UpvarArgs& args) = 0;
    virtual void EmitStorUpvar(LLOp op, const UpvarArgs& args) = 0;
    virtual void EmitSlice(uint16_t base_reg, uint16_t index_reg, uint16_t dest_reg) = 0;
    virtual void EmitSliceEs(uint16_t src_reg, uint16_t dest_reg, uint32_t cells) = 0;
    virtual void EmitSliceFlat(const SliceFlatArgs& op) = 0;
    virtual void EmitIdxAddr(const IdxAddrArgs& args) = 0;
    virtual void EmitCopyArray(LLOp op, uint16_t src_reg, uint16_t dest_reg, uint32_t bytes) = 0;
    virtual void EmitCopyObj(uint16_t src_reg, uint16_t dest_reg, uint32_t bytes) = 0;
    virtual void EmitArrayToFlat(uint16_t src_reg, uint16_t dest_reg) = 0;
    virtual void EmitAddrFld(uint16_t src_reg, uint16_t dest_reg, uint32_t offset) = 0;
    virtual void EmitSwitchChain(uint16_t val_reg, uint32_t def_block,
                                 const std::span<const SwitchCaseEntry>& cases) = 0;
    virtual void EmitSwitchTable(uint16_t val_reg, uint32_t def_block,
                                 const std::span<const SwitchCaseEntry>& cases) = 0;
    virtual void EmitCallee(uint16_t dest_reg) = 0;

    // Errors.
    struct DeallocThunk;
    virtual void EmitDeallocThunk(DeallocThunk* thunk) = 0;

    struct BoundsErrorThunk;
    virtual void EmitBoundsErrorThunk(BoundsErrorThunk* thunk) = 0;

    struct DeferredErrorThunk;
    virtual void EmitDeferredErrorThunk(DeferredErrorThunk* thunk) = 0;

    struct CallThunk;
    virtual void EmitCallThunk(CallThunk* thunk) = 0;

    struct IndirectCallThunk;
    virtual void EmitIndirectCallThunk(IndirectCallThunk* thunk) = 0;

  protected:
    BoundsErrorThunk& AddBoundsErrorThunk();
    DeferredErrorThunk& AddDeferredErrorThunk();
    IndirectCallThunk& AddIndirectCallThunk(uint16_t fn_reg);

    bool IsBlockEmitted(size_t block_idx) const {
        return block_labels_[block_idx].bound();
    }

    bool TryEmitSwitchTable(uint16_t val_reg, uint32_t def_block,
                            const std::span<const SwitchCaseEntry>& cases);

  public:
    // Helpers.
    static void* FindEntryFp();
    static void InvokeReportError(int err);
    static void InvokeReportTimeout();
    static void DispatchDeferredReport();

  protected:
    // Helpers.
    static void* LazyCompileThunk(Runtime* cx, uint32_t method_index, uint8_t* pc);
    static CompiledFunction* IndirectCompileThunk(Runtime* cx, MethodInfo* method);
    static void PatchCallThunk(uint8_t* pc, void* target);

  protected:

    // Map a return address (i.e. an exit point from a function) to its source
    // cip. This lets us avoid tracking the cip during runtime. These are
    // sorted by definition since we assemble and emit in forward order.
    void EmitCipMapping(const uint8_t* cip) {
        CipMapEntry entry;
        entry.cipoffs = (uint32_t)(cip - code_start_);
        entry.pcoffs = masm.pc();
        cip_map_.push_back(entry);
    }

    bool IsNextBlock(uint32_t block_index);

  protected:
    struct ErrorThunk;
    void EmitErrorThunk(ErrorThunk* path);

    void ReportError(int err);

  protected:
    Environment* env_;
    const v2::ReturnStubs& stubs_;
    Runtime* rt_;
    Runtime* context_;
    SmxImage* image_;
    ke::RefPtr<MethodInfo> method_info_;
    LLCode* ll_;
    ke::RefPtr<ControlFlowGraph> graph_;
    uint32_t pcode_start_;
    const uint8_t* code_start_;
    const uint8_t* op_cip_;
    const LLBlock* block_ = nullptr;

    MacroAssembler masm;

    struct CallThunk {
        explicit CallThunk(uint32_t method_index)
          : method_index(method_index)
        {}
        CallThunk(CallThunk&& other) = default;
        CallThunk& operator =(CallThunk& other) = default;

        PatchCodeLabel label;
        uint32_t method_index;
    };
    std::vector<CallThunk> call_thunks_;

    struct IndirectCallThunk {
        explicit IndirectCallThunk(const uint8_t* cip, uint16_t fn_reg)
          : cip(cip),
            fn_reg(fn_reg)
        {}
        Label label;
        Label return_to;
        const uint8_t* cip;
        uint16_t fn_reg;
    };
    std::vector<IndirectCallThunk> indirect_call_thunks_;

    struct ErrorThunk {
        explicit ErrorThunk(const uint8_t* cip, int err)
          : cip(cip), err(err)
        {}
        Label label;
        const uint8_t* cip;
        int err;
    };
    std::vector<ErrorThunk> error_thunks_;

    struct BoundsErrorThunk {
        explicit BoundsErrorThunk(const uint8_t* cip)
          : cip(cip)
        {}

        Label label;
        const uint8_t* cip;
        std::variant<Register, uint32_t> index;
        std::variant<Register, uint32_t> limit;
    };
    std::vector<BoundsErrorThunk> bounds_errors_;

    struct DeferredErrorThunk {
        explicit DeferredErrorThunk(const uint8_t* cip)
          : cip(cip)
        {}
        Label label;
        const uint8_t* cip;
    };
    std::vector<DeferredErrorThunk> deferred_errors_;

    struct DeallocThunk {
        DeallocThunk(Register obj_reg, std::optional<Register> save_reg, const uint8_t* cip)
          : obj_reg(obj_reg),
            save_reg(save_reg),
            cip(cip)
        {}

        Label label;
        Label return_label;
        Register obj_reg;
        std::optional<Register> save_reg;
        const uint8_t* cip;
    };
    std::vector<DeallocThunk> dealloc_thunks_;

    // Debugging.
    Label debug_break_;
    std::string debug_name_;

    std::vector<BackwardJump> backward_jumps_;
    std::vector<CipMapEntry> cip_map_;
    ke::FixedArray<Label> block_labels_;
    ke::FixedArray<PatchCodeLabel> block_addresses_;
};

} // namespace sp::v2
