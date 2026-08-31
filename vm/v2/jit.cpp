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
//
#include "v2/jit.h"

#include <stdio.h>

#include "binary-reader.h"
#include "debug-metadata.h"
#include "environment.h"
#include "linking.h"
#include "v2/method-info.h"
#include "v2/opcodes.h"
#include "v2/lowering/ll-op.h"
#include "v2/lowering/llcode.h"
#include "v2/lowering/lowering.h"
#include "v2/runtime.h"
#include "stack-frames.h"
#include "watchdog_timer.h"
#if defined(KE_ARCH_X86)
# include "x86/jit_x86.h"
#elif defined(KE_ARCH_X64)
# include "x64/jit_x64.h"
#endif

namespace sp::v2 {

using namespace SourcePawn;

#define __ masm.

CompilerBase::CompilerBase(Runtime* rt, MethodInfo* method)
 : env_(Environment::get()),
   stubs_(env_->stubs()->return_stubs_v2()),
   rt_(rt),
   context_(rt),
   image_(rt_->image()),
   method_info_(method),
   ll_(method->llcode()),
   pcode_start_(0),
   code_start_(nullptr),
   op_cip_(nullptr)
{}

CompilerBase::~CompilerBase() {
    method_info_->ClearCompilerCache();
}

bool CompilerBase::IsNextBlock(uint32_t block_index) {
    if (!block_)
        return false;
    uint32_t current_index = block_ - method_info_->llcode()->blocks().begin();
    return current_index + 1 == block_index;
}

bool CompilerBase::Compile(Runtime* cx, RefPtr<MethodInfo> method) {
    Environment* env = Environment::get();
    if (!env->EnsureStubs())
        return false;

    if (!method->llcode()) {
        ke::RefPtr<ControlFlowGraph> graph = method->BuildGraph();
        if (!graph)
            return false;

        std::unique_ptr<LLCode> code = LowerMethod(graph, method.get());
        method->set_llcode(std::move(code));
    }

    Compiler cc(cx, method);

    CompiledFunction* fun = cc.Emit();
    if (!fun)
        return false;

    method->setCompiledFunction(fun);
    return fun;
}

CompiledFunction* CompilerBase::Emit() {
    if (!env_->EnsureStubs()) {
        ReportError(SP_ERROR_OUT_OF_MEMORY);
        return nullptr;
    }

    pcode_start_ = method_info_->pcode_offset();
    code_start_ = ll_->bytes();

    std::string function_name;
    if (const char* name = rt_->image()->LookupFunction(pcode_start_))
        function_name = name;
    else
        function_name = "anonymous_" + std::to_string(pcode_start_);

    debug_name_ = std::string(rt_->Name()) + "::" + function_name;

#if defined JIT_SPEW
    Environment::get()->debugger()->OnDebugSpew("Compiling function %s\n", debug_name_.c_str());
#endif

    CodeDebugMap debug_map;

    // DWARF has special tags for marking the prologue/epilogue, but they're not exposed
    // by the jitdump format. As that information is useful for humans, we emit a couple
    // of fake source file mappings to frame the actual function body.
    debug_map.push_back({masm.pc(), "<prologue>", 0});

    FrameInfo frame;
    frame.num_regs = ll_->num_regs();
    frame.frame_size = (frame.num_regs + ll_->max_callee_args()) * sizeof(cell_t);
    frame.num_params = method_info_->arg_types().size();
    frame.callee_regs = frame.num_regs - frame.num_params;

    EmitPrologue(frame);

    const auto& blocks = ll_->blocks();
    block_labels_ = ke::FixedArray<Label>(blocks.size());
    block_addresses_ = ke::FixedArray<PatchCodeLabel>(blocks.size());
    for (size_t i = 0; i < blocks.size(); i++) {
        __ bind(&block_labels_[i]);
        __ bind(&block_addresses_[i]);
        if (!CompileBlock(blocks[i]))
            return nullptr;
    }

    debug_map.push_back({masm.pc(), "<epilogue>", 0});

    for (auto& call : call_thunks_) {
        __ bind(&call.label);
        EmitCallThunk(&call);
    }

    for (auto& call : indirect_call_thunks_) {
        __ bind(&call.label);
        EmitIndirectCallThunk(&call);
    }

    for (auto& error : bounds_errors_) {
        __ bind(&error.label);
        EmitBoundsErrorThunk(&error);
    }

    for (auto& thunk : deferred_errors_) {
        __ bind(&thunk.label);
        EmitDeferredErrorThunk(&thunk);
    }

    for (auto& thunk : dealloc_thunks_) {
        __ bind(&thunk.label);
        EmitDeallocThunk(&thunk);
    }

    // Out-of-line paths can emit new error thunks, so we process error thunks
    // last. JumpOnError must not be called after this.
    for (auto& thunk : error_thunks_) {
        __ bind(&thunk.label);
        EmitErrorThunk(&thunk);
    }

    // For each backward jump, emit a little thunk so we can exit from a timeout.
    // Track the offset of where the thunk is, so the watchdog timer can patch it.
    for (size_t i = 0; i < backward_jumps_.size(); i++) {
        BackwardJump& jump = backward_jumps_[i];
        jump.timeout_offset = masm.pc();
        __ call(ExternalAddress(stubs_.throw_timeout));
        EmitCipMapping(jump.cip);
    }

    // perf's translation of the jitdump debug mappings to DWARF ignores the last
    // record, so we emit a bonus one here so that the epilogue marker is included.
    debug_map.push_back({masm.pc(), "<end>", 0});

    LinkedCode code = LinkCode(env_, masm, debug_name_.c_str(), debug_map);
    if (!code.entry) {
        ReportError(SP_ERROR_OUT_OF_MEMORY);
        return nullptr;
    }

    std::unique_ptr<FixedArray<LoopEdge>> edges(new FixedArray<LoopEdge>(backward_jumps_.size()));
    for (size_t i = 0; i < backward_jumps_.size(); i++) {
        const BackwardJump& jump = backward_jumps_[i];
        edges->at(i).offset = jump.pc;
        edges->at(i).disp32 = int32_t(jump.timeout_offset) - int32_t(jump.pc);
    }

    std::unique_ptr<FixedArray<CipMapEntry>> cipmap(new FixedArray<CipMapEntry>(cip_map_.size()));
    memcpy(cipmap->buffer(), cip_map_.data(), cip_map_.size() * sizeof(CipMapEntry));

    return new CompiledFunction(code, edges.release(), cipmap.release());
}

bool CompilerBase::CompileBlock(const LLBlock& block) {
    BinaryReader reader(block.bytes.data(), block.bytes.data() + block.bytes.size());

    block_ = &block;

    while (reader.more()) {
        op_cip_ = reader.cursor();
        LLOp op = (LLOp)reader.read<uint16_t>();
        switch (op) {
            case LL_LOAD_CONST: {
                cell_t val = reader.read<cell_t>();
                uint16_t reg = reader.read<uint16_t>();
                EmitLoadConst(reg, val);
                break;
            }
            case LL_LOAD_CONST_I64: {
                int64_t val = reader.read<int64_t>();
                uint16_t reg = reader.read<uint16_t>();
                EmitLoadConst64(reg, val);
                break;
            }
            case LL_ADDR_S: {
                uint16_t src = reader.read<uint16_t>();
                uint16_t dest = reader.read<uint16_t>();
                EmitAddr(src, dest);
                break;
            }
            case LL_RETN:
            case LL_RETN_A:
            {
                uint16_t reg = reader.read<uint16_t>();
                EmitRetn(op, reg);
                break;
            }
            case LL_RETV:
                EmitRetn(op, {});
                break;
            case LL_MOVE:
            case LL_MOVE_I64:
            case LL_STOR_S_A:
            {
                uint16_t src = reader.read<uint16_t>();
                uint16_t dest = reader.read<uint16_t>();
                EmitMove(op, src, dest);
                break;
            }
            case LL_CALL:
            {
                const smx_rtti_method* method = reader.read<const smx_rtti_method*>();
                uint8_t nargs = reader.read<uint8_t>();
                uint16_t dest = reader.read<uint16_t>();
                uint32_t method_index = method - rt_->image()->GetMethod(0);

                std::vector<uint16_t> args(nargs);
                for (uint8_t i = 0; i < nargs; i++)
                    args[i] = reader.read<uint16_t>();

                EmitScriptedCall(method_index, nargs, dest, args);
                break;
            }
            case LL_CALLI:
            {
                uint16_t fn_reg = reader.read<uint16_t>();
                uint8_t nargs = reader.read<uint8_t>();
                uint16_t dest = reader.read<uint16_t>();

                std::vector<uint16_t> args(nargs);
                for (uint8_t i = 0; i < nargs; i++)
                    args[i] = reader.read<uint16_t>();

                EmitIndirectCall(fn_reg, nargs, dest, args);
                break;
            }
            case LL_NTVCALL:
            case LL_NTVCALL_VA:
            {
                uint32_t native_index = reader.read<uint32_t>();
                uint8_t nargs = reader.read<uint8_t>();

                uint16_t spread_reg = LL_INVALID_REG;
                if (op == LL_NTVCALL_VA)
                    spread_reg = reader.read<uint16_t>();

                uint16_t dest = reader.read<uint16_t>();
                std::vector<uint16_t> args(nargs);
                for (uint8_t i = 0; i < nargs; i++)
                    args[i] = reader.read<uint16_t>();
                EmitNativeCall(native_index, nargs, dest, args, spread_reg);
                break;
            }
            case LL_JUMP: {
                reader.read<int32_t>();
                EmitJump(block.successors[0]);
                break;
            }
            case LL_JZER:
            case LL_JNZ: {
                uint16_t reg = reader.read<uint16_t>();
                reader.read<int32_t>();
                EmitJump(op, reg, block.successors[1]);
                break;
            }
            case LL_JEQ:
            case LL_JNEQ:
            case LL_JSLESS:
            case LL_JSLEQ:
            case LL_JSGRTR:
            case LL_JSGEQ: {
                uint16_t reg_a = reader.read<uint16_t>();
                uint16_t reg_b = reader.read<uint16_t>();
                reader.read<int32_t>();
                EmitJumpCmp(op, reg_a, reg_b, block.successors[1]);
                break;
            }
            case LL_EQ_I32:
            case LL_NEQ_I32:
            case LL_SLESS_I32:
            case LL_SLEQ_I32:
            case LL_SGRTR_I32:
            case LL_SGEQ_I32: {
                uint16_t reg_a = reader.read<uint16_t>();
                uint16_t reg_b = reader.read<uint16_t>();
                uint16_t dest = reader.read<uint16_t>();
                EmitCmpI32(op, reg_a, reg_b, dest);
                break;
            }
            case LL_ADD_I32:
            case LL_SUB_I32:
            case LL_SMUL_I32:
            case LL_XOR_I32:
            case LL_OR_I32:
            case LL_AND_I32:
            case LL_SHL_I32:
            case LL_SHR_I32:
            case LL_SSHR_I32:
            {
                uint16_t lhs_reg = reader.read<uint16_t>();
                uint16_t rhs_reg = reader.read<uint16_t>();
                uint16_t dest = reader.read<uint16_t>();
                EmitBasicAlu(op, lhs_reg, rhs_reg, dest);
                break;
            }
            case LL_INVERT_I32:
            case LL_NEG_I32:
            case LL_NOT_I32:
            case LL_TEST_I32:
            case LL_CVT_I16:
            {
                uint16_t src_reg = reader.read<uint16_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                EmitUnaryAlu(op, src_reg, dest_reg);
                break;
            }
            case LL_SDIV_I32:
            case LL_SMOD_I32:
            {
                uint16_t lhs_reg = reader.read<uint16_t>();
                uint16_t rhs_reg = reader.read<uint16_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                EmitSdivI32(op, lhs_reg, rhs_reg, dest_reg);
                break;
            }
            case LL_EQ_F32:
            case LL_NEQ_F32:
            case LL_LESS_F32:
            case LL_LEQ_F32:
            case LL_GRTR_F32:
            case LL_GEQ_F32:
            {
                uint16_t reg_a = reader.read<uint16_t>();
                uint16_t reg_b = reader.read<uint16_t>();
                uint16_t dest = reader.read<uint16_t>();
                EmitCompareFloat(op, reg_a, reg_b, dest);
                break;
            }
            case LL_ADD_F32:
            case LL_SUB_F32:
            case LL_MUL_F32:
            case LL_DIV_F32:
            case LL_MOD_F32:
            {
                uint16_t lhs_reg = reader.read<uint16_t>();
                uint16_t rhs_reg = reader.read<uint16_t>();
                uint16_t dest = reader.read<uint16_t>();
                EmitBinaryFloatOp(op, lhs_reg, rhs_reg, dest);
                break;
            }
            case LL_CVT_F32:
            case LL_TEST_F32:
            case LL_NEG_F32:
            {
                uint16_t src_reg = reader.read<uint16_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                EmitUnaryFloatOp(op, src_reg, dest_reg);
                break;
            }
            case LL_EQ_I64:
            case LL_NEQ_I64:
            case LL_SLESS_I64:
            case LL_SLEQ_I64:
            case LL_SGRTR_I64:
            case LL_SGEQ_I64: {
                uint16_t reg_a = reader.read<uint16_t>();
                uint16_t reg_b = reader.read<uint16_t>();
                uint16_t dest = reader.read<uint16_t>();
                EmitCmpI64(op, reg_a, reg_b, dest);
                break;
            }
            case LL_ADD_I64:
            case LL_SUB_I64:
            case LL_SMUL_I64:
            case LL_XOR_I64:
            case LL_OR_I64:
            case LL_AND_I64:
            case LL_SHL_I64:
            case LL_SHR_I64:
            case LL_SSHR_I64:
            {
                uint16_t lhs_reg = reader.read<uint16_t>();
                uint16_t rhs_reg = reader.read<uint16_t>();
                uint16_t dest = reader.read<uint16_t>();
                EmitBinaryI64(op, lhs_reg, rhs_reg, dest);
                break;
            }
            case LL_INVERT_I64:
            case LL_NEG_I64:
            case LL_CVT_I64:
            case LL_TEST_I64:
            case LL_TRUNCATE_I64:
            {
                uint16_t src_reg = reader.read<uint16_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                EmitUnaryI64(op, src_reg, dest_reg);
                break;
            }
            case LL_SDIV_I64:
            case LL_SMOD_I64:
            {
                uint16_t lhs_reg = reader.read<uint16_t>();
                uint16_t rhs_reg = reader.read<uint16_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                EmitSdivI64(op, lhs_reg, rhs_reg, dest_reg);
                break;
            }
            case LL_NEWARRAY:
            {
                auto td = reader.read<const TypeDesc*>();
                uint16_t size_reg = reader.read<uint16_t>();
                uint16_t dest = reader.read<uint16_t>();
                EmitNewArray(td, size_reg, dest);
                break;
            }
            case LL_NEWFIXEDARRAY: {
                auto td = reader.read<const TypeDesc*>();
                uint16_t dest = reader.read<uint16_t>();
                uint32_t size = td->array_size();
                EmitNewFixedArray(td, dest, size);
                break;
            }
            case LL_NEWBULKARRAY: {
                uint8_t dims = reader.read<uint8_t>();
                auto td = reader.read<const TypeDesc*>();
                uint16_t size_reg = reader.read<uint16_t>();
                uint16_t dest = reader.read<uint16_t>();
                EmitNewBulkArray(dims, td, size_reg, dest);
                break;
            }
            case LL_ADDREF: {
                uint16_t reg = reader.read<uint16_t>();
                EmitAddRef(reg);
                break;
            }
            case LL_RELEASE: {
                uint16_t reg = reader.read<uint16_t>();
                EmitRelease(reg);
                break;
            }
            case LL_LOAD_STR: {
                uint32_t index = reader.read<uint16_t>();
                uint16_t dest = reader.read<uint16_t>();
                uint32_t str_addr = rt_->GetStringAddr(index);
                EmitLoadInternedObj(str_addr, dest);
                break;
            }
            case LL_LOAD_FN: {
                uint32_t method_index = reader.read<uint32_t>();
                uint16_t dest = reader.read<uint16_t>();
                auto method = rt_->AcquireMethod(method_index);
                Handle<SpFunction> fn = method->GetFunction();
                if (!fn)
                    return false;

                auto fn_addr = env_->virt_mem().ToLocalAddr(fn.get());
                EmitLoadInternedObj(fn_addr, dest);
                break;
            }
            case LL_GETFUNCID: {
                uint16_t src_reg = reader.read<uint16_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                EmitGetFuncId(src_reg, dest_reg);
                break;
            }
            case LL_LOAD_I_U8:
            case LL_LOAD_I_I32:
            case LL_LOAD_I_F32:
            case LL_LOAD_I_I64:
            {
                uint16_t src_reg = reader.read<uint16_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                EmitLoadI(op, src_reg, dest_reg);
                break;
            }
            case LL_STOR_I_U8:
            case LL_STOR_I_I32:
            case LL_STOR_I_F32:
            case LL_STOR_I_I64:
            case LL_STOR_I_A:
            {
                uint16_t addr_reg = reader.read<uint16_t>();
                uint16_t val_reg = reader.read<uint16_t>();
                EmitStorI(op, addr_reg, val_reg);
                break;
            }
            case LL_LOAD_FLD_X32:
            case LL_LOAD_FLD_X64:
            case LL_LOAD_FLD_A:
            {
                uint32_t offset = reader.read<uint32_t>();
                uint16_t addr_reg = reader.read<uint16_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                EmitLoadFld(op, addr_reg, offset, dest_reg);
                break;
            }
            case LL_STOR_FLD_X32:
            case LL_STOR_FLD_X64:
            case LL_STOR_FLD_A:
            {
                uint32_t offset = reader.read<uint32_t>();
                uint16_t addr_reg = reader.read<uint16_t>();
                uint16_t val_reg = reader.read<uint16_t>();
                EmitStorFld(op, addr_reg, offset, val_reg);
                break;
            }
            case LL_LOAD_GLB_X32:
            case LL_LOAD_GLB_X64:
            case LL_LOAD_GLB_A:
            case LL_ADDR_GLB:
            {
                uint16_t index = reader.read<uint16_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                uint32_t addr = rt_->GetGlobalAddr(index);
                EmitLoadGlb(op, addr, dest_reg);
                break;
            }
            case LL_STOR_GLB_X32:
            case LL_STOR_GLB_X64:
            case LL_STOR_GLB_A:
            {
                uint16_t index = reader.read<uint16_t>();
                uint16_t val_reg = reader.read<uint16_t>();
                uint32_t addr = rt_->GetGlobalAddr(index);
                EmitStorGlb(op, addr, val_reg);
                break;
            }
            case LL_FILLARRAY: {
                uint32_t data_offset = reader.read<uint32_t>();
                uint16_t addr_reg = reader.read<uint16_t>();

                BinaryReader br = image_->GetDataReader(data_offset);
                auto data_bytes = br.readCompactUint32();
                assert(data_bytes);

                EmitFillArray(addr_reg, br.cursor(), *data_bytes);
                break;
            }
            case LL_FILLARRAY_FLAT: {
                uint32_t data_offs = reader.read<uint32_t>();
                const TypeDesc* td = reader.read<const TypeDesc*>();
                uint16_t addr_reg = reader.read<uint16_t>();

                BinaryReader br = image_->GetDataReader(data_offs);
                auto data_bytes = br.readCompactUint32();
                assert(data_bytes);

                auto elt_size = td->array_elt()->element_size();
                assert(*data_bytes % elt_size == 0);
                [[maybe_unused]] auto elt_count = *data_bytes / elt_size;
                assert(elt_count <= td->array_size());

                EmitFillArrayFlat(addr_reg, br.cursor(), *data_bytes);
                break;
            }
            case LL_LOAD_ELEM_FLAT_I32:
            case LL_LOAD_ELEM_FLAT_F32:
            case LL_LOAD_ELEM_FLAT_I64:
            case LL_LOAD_ELEM_FLAT_U8:
            case LL_LOAD_ELEM_FLAT_I8:
            case LL_LOAD_ELEM_FLAT_I16:
            {
                auto args = reader.read<LoadElemFlatArgs>();
                EmitLoadElemFlat(op, args);
                break;
            }
            case LL_LOAD_ELEM_FLAT_I_I32:
            case LL_LOAD_ELEM_FLAT_I_F32:
            case LL_LOAD_ELEM_FLAT_I_I64:
            case LL_LOAD_ELEM_FLAT_I_U8:
            case LL_LOAD_ELEM_FLAT_I_I8:
            case LL_LOAD_ELEM_FLAT_I_I16:
            {
                auto args = reader.read<LoadElemFlatArgs>();
                EmitLoadElemFlatI(op, args);
                break;
            }
            case LL_STOR_ELEM_FLAT_I32:
            case LL_STOR_ELEM_FLAT_F32:
            case LL_STOR_ELEM_FLAT_I64:
            case LL_STOR_ELEM_FLAT_I8:
            case LL_STOR_ELEM_FLAT_I16:
            {
                auto args = reader.read<StorElemFlatArgs>();
                EmitStorElemFlat(op, args);
                break;
            }
            case LL_STOR_ELEM_FLAT_I_I32:
            case LL_STOR_ELEM_FLAT_I_F32:
            case LL_STOR_ELEM_FLAT_I_I64:
            case LL_STOR_ELEM_FLAT_I_I8:
            case LL_STOR_ELEM_FLAT_I_I16:
            {
                auto args = reader.read<StorElemFlatArgs>();
                EmitStorElemFlatI(op, args);
                break;
            }
            case LL_IDXADDR_FLAT: {
                auto args = reader.read<IdxAddrFlatArgs>();
                EmitIdxAddrFlat(args);
                break;
            }
            case LL_SLICE: {
                uint16_t base_reg = reader.read<uint16_t>();
                uint16_t index_reg = reader.read<uint16_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                EmitSlice(base_reg, index_reg, dest_reg);
                break;
            }
            case LL_SLICE_ES: {
                uint32_t cell_count = reader.read<uint32_t>();
                uint16_t src_reg = reader.read<uint16_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                EmitSliceEs(src_reg, dest_reg, cell_count);
                break;
            }
            case LL_SLICE_FLAT: {
                auto args = reader.read<SliceFlatArgs>();
                EmitSliceFlat(args);
                break;
            }
            case LL_IDXADDR: {
                auto args = reader.read<IdxAddrArgs>();
                EmitIdxAddr(args);
                break;
            }
            case LL_LOAD_ELEM_I32:
            case LL_LOAD_ELEM_F32:
            case LL_LOAD_ELEM_I64:
            case LL_LOAD_ELEM_U8:
            case LL_LOAD_ELEM_I8:
            case LL_LOAD_ELEM_I16:
            case LL_LOAD_ELEM_A:
            {
                uint16_t base_reg = reader.read<uint16_t>();
                uint16_t index_reg = reader.read<uint16_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                EmitLoadElem(op, base_reg, index_reg, dest_reg);
                break;
            }
            case LL_STOR_ELEM_I32:
            case LL_STOR_ELEM_F32:
            case LL_STOR_ELEM_I64:
            case LL_STOR_ELEM_I8:
            case LL_STOR_ELEM_I16:
            case LL_STOR_ELEM_A:
            {
                uint16_t base_reg = reader.read<uint16_t>();
                uint16_t index_reg = reader.read<uint16_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                EmitStorElem(op, base_reg, index_reg, dest_reg);
                break;
            }
            case LL_COPYARRAY:
            case LL_COPYARRAY_FLAT:
            {
                uint32_t bytes = reader.read<uint32_t>();
                uint16_t src_reg = reader.read<uint16_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                EmitCopyArray(op, src_reg, dest_reg, bytes);
                break;
            }
            case LL_ARRAY_TO_FLAT: {
                uint16_t src_reg = reader.read<uint16_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                EmitArrayToFlat(src_reg, dest_reg);
                break;
            }
            case LL_ADDR_FLD: {
                uint32_t offset = reader.read<uint32_t>();
                uint16_t base = reader.read<uint16_t>();
                uint16_t dest = reader.read<uint16_t>();
                EmitAddrFld(base, dest, offset);
                break;
            }
            case LL_SWITCH: {
                uint16_t val_reg = reader.read<uint16_t>();
                uint32_t ncases = reader.read<uint32_t>();
                /* default offset */ reader.read<uint32_t>();
                auto cases = reader.getSpan<SwitchCaseEntry>(ncases);

                uint32_t default_block = block.successors[0];
                if (cases.size() == 0) {
                    EmitJump(default_block);
                    break;
                }

                if (!TryEmitSwitchTable(val_reg, default_block, cases))
                    EmitSwitchChain(val_reg, default_block, cases);
                break;
            }
            case LL_COPYOBJ: {
                uint32_t bytes = reader.read<uint32_t>();
                uint16_t src_reg = reader.read<uint16_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                EmitCopyObj(src_reg, dest_reg, bytes);
                break;
            }
            case LL_NEWOBJ: {
                auto td = reader.read<const TypeDesc*>();
                uint16_t dest_reg = reader.read<uint16_t>();
                EmitNewObj(td, dest_reg);
                break;
            }
            case LL_NEWCLOSURE: {
                auto closure_td = reader.read<const TypeDesc*>();
                auto method_id = reader.read<uint32_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                auto method = rt_->AcquireMethod(method_id);
                EmitNewClosure(closure_td, method.get(), dest_reg);
                break;
            }
            case LL_CALLEE: {
                uint16_t dest_reg = reader.read<uint16_t>();
                EmitCallee(dest_reg);
                break;
            }
            case LL_STOR_UPVAR_X32:
            case LL_STOR_UPVAR_X64:
            case LL_STOR_UPVAR_A: {
                auto args = reader.read<UpvarArgs>();
                args.slot = SpFunction::OffsetOfSlot(args.slot);
                EmitStorUpvar(op, args);
                break;
            }
            case LL_ADDR_UPVAR:
            case LL_LOAD_UPVAR_X32:
            case LL_LOAD_UPVAR_X64:
            case LL_LOAD_UPVAR_A: {
                auto args = reader.read<UpvarArgs>();
                args.slot = SpFunction::OffsetOfSlot(args.slot);
                EmitLoadUpvar(op, args);
                break;
            }

            default:
                fprintf(stderr, "Unimplemented opcode: %s\n", GetLLOpName(op));
                assert(false);
                ReportError(SP_ERROR_NOT_RUNNABLE);
                return false;
        }
    }

    block_ =  nullptr;
    return true;
}

bool CompilerBase::TryEmitSwitchTable(uint16_t val_reg, uint32_t def_block,
                                      const std::span<const SwitchCaseEntry>& cases)
{
    // Don't bother with switch tables for extremely short cases.
    if (cases.size() <= 2)
        return false;

    // We normalize the table to zero, which means adding -N to each value,
    // where N is the smallest value. This means if N is INT_MIN, we'd
    // overflow, so we must reject a table for these.
    const auto& first_case = cases[0];
    if (first_case.value == INT_MIN)
        return false;

    // Do a quick check to see if the bounds are definitely NOT sequential.
    const auto& last_case = cases[cases.size() - 1];
    if (first_case.value >= last_case.value)
        return false;
    if (uint32_t(last_case.value - first_case.value) + 1 != cases.size())
        return false;

    // Now do the sequential check.
    cell_t iter = first_case.value;
    for (const auto& entry : cases) {
        if (entry.value != iter++)
            return false;
    }

    EmitSwitchTable(val_reg, def_block, cases);
    return true;
}

void CompilerBase::EmitErrorThunk(ErrorThunk* thunk) {
    if (thunk->err == 0) {
        __ call(ExternalAddress(stubs_.report_error));
    } else if (thunk->err == -1) {
        __ call(ExternalAddress(stubs_.return_reported_error));
    } else {
        assert(thunk->err < SP_MAX_ERROR_CODES);
        __ call(ExternalAddress(stubs_.throw_error_code[thunk->err]));
    }

    EmitCipMapping(thunk->cip);
}

auto CompilerBase::AddBoundsErrorThunk() -> BoundsErrorThunk& {
    bounds_errors_.emplace_back(op_cip_);
    return bounds_errors_.back();
}

auto CompilerBase::AddDeferredErrorThunk() -> DeferredErrorThunk& {
    deferred_errors_.emplace_back(op_cip_);
    return deferred_errors_.back();
}

auto CompilerBase::AddIndirectCallThunk(uint16_t fn_reg) -> IndirectCallThunk& {
    indirect_call_thunks_.emplace_back(op_cip_, fn_reg);
    return indirect_call_thunks_.back();
}

void CompilerBase::ReportError(int err) {
    env_->ReportError(err);
}

CompiledFunction* CompilerBase::IndirectCompileThunk(Runtime* cx, MethodInfo* method) {
    // If the watchdog timer has declared a timeout, we must process it now,
    // and possibly refuse to compile, since otherwise we will compile a
    // function that is not patched for timeouts.
    Environment* env = Environment::get();
    if (!env->watchdog()->HandleInterrupt()) {
        env->ReportError(SP_ERROR_TIMEOUT);
        return nullptr;
    }

    if (!method->jit() && !Compile(cx, method))
        return nullptr;

    return method->jit();
}

void* CompilerBase::LazyCompileThunk(Runtime* cx, uint32_t method_index, uint8_t* pc) {
    RefPtr<MethodInfo> method = cx->runtime()->AcquireMethod(method_index);
    if (!method) {
        // Should be impossible.
        Environment::get()->ReportError(SP_ERROR_INVALID_INSTRUCTION);
        return nullptr;
    }

    auto fn = IndirectCompileThunk(cx, method.get());
    assert(fn);

    /* Right now, we always keep the code RWE */
    PatchCallThunk(pc, fn->GetEntryAddress());
    return fn->GetEntryAddress();
}

// Find the |ebp| associated with the entry frame. We use this to drop out of
// the entire scripted call stack.
void* CompilerBase::FindEntryFp() {
    void* fp = nullptr;

    for (JitFrameIterator iter(Environment::get()); !iter.done(); iter.next()) {
        FrameLayout* frame = iter.frame();
        if (frame->frame_type() == JitFrameType::Entry)
            break;
        fp = frame->prev_fp;
    }

    assert(fp);
    return fp;
}

// Exit frame is a JitExitFrameForHelper.
void CompilerBase::InvokeReportError(int err) {
    Environment::get()->ReportError(err);
}

// Exit frame is a JitExitFrameForHelper. This is a special function since we
// have to notify the watchdog timer that we're unblocked.
void CompilerBase::InvokeReportTimeout() {
    Environment::get()->watchdog()->NotifyTimeoutReceived();
    InvokeReportError(SP_ERROR_TIMEOUT);
}

// Exit frame is a JitExitFrameForHelper.
void CompilerBase::DispatchDeferredReport() {
    Environment::get()->DispatchDeferredReport();
}

} // namespace sp::v2
