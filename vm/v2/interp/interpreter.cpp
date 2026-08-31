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
#include "v2/interp/interpreter.h"

#include <cstddef>
#include <fenv.h>
#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include <limits>
#include <memory>
#include <utility>

#include <amtl/am-float.h>
#include "debugging.h"
#include "environment.h"
#include "v2/interp/llcode.h"
#include "v2/interp/ll-op.h"
#include "v2/interp/lowering.h"
#include "v2/method-info.h"
#include "v2/pcode-reader.h"
#include "v2/runtime-helpers.h"
#include "v2/runtime.h"
#include "objects.h"


#define BINARY_OP_I32(op, oper) \
            case op: { \
                uint16_t a = reader_.read<uint16_t>(); \
                uint16_t b = reader_.read<uint16_t>(); \
                uint16_t dest = reader_.read<uint16_t>(); \
                vregs_[dest] = vregs_[a] oper vregs_[b]; \
                break; \
            }

#define BINARY_OP_F32(op, oper) \
            case op: { \
                uint16_t a = reader_.read<uint16_t>(); \
                uint16_t b = reader_.read<uint16_t>(); \
                uint16_t dest = reader_.read<uint16_t>(); \
                vregs_[dest] = sp_ftoc(sp_ctof(vregs_[a]) oper sp_ctof(vregs_[b])); \
                break; \
            }

#define COMPARE_OP_F32(op, oper) \
            case op: { \
                uint16_t a = reader_.read<uint16_t>(); \
                uint16_t b = reader_.read<uint16_t>(); \
                uint16_t dest = reader_.read<uint16_t>(); \
                vregs_[dest] = (sp_ctof(vregs_[a]) oper sp_ctof(vregs_[b])) ? 1 : 0; \
                break; \
            }

#define BINARY_OP_I64(op, oper) \
            case op: { \
                uint16_t a = reader_.read<uint16_t>(); \
                uint16_t b = reader_.read<uint16_t>(); \
                uint16_t dest = reader_.read<uint16_t>(); \
                *reinterpret_cast<int64_t*>(&vregs_[dest]) = \
                    *reinterpret_cast<int64_t*>(&vregs_[a]) oper *reinterpret_cast<int64_t*>(&vregs_[b]); \
                break; \
            }

#define COMPARE_OP_I64(op, oper) \
            case op: { \
                uint16_t a = reader_.read<uint16_t>(); \
                uint16_t b = reader_.read<uint16_t>(); \
                uint16_t dest = reader_.read<uint16_t>(); \
                vregs_[dest] = (*reinterpret_cast<int64_t*>(&vregs_[a]) oper *reinterpret_cast<int64_t*>(&vregs_[b])) ? 1 : 0; \
                break; \
            }

#define UNARY_JUMP_OP(op, oper) \
            case op: { \
                uint16_t a = reader_.read<uint16_t>(); \
                cell_t offset = reader_.read<int32_t>(); \
                if (vregs_[a] oper 0) { \
                    if (offset < (cell_t)(insn_begin - ll_code)) { \
                        if (!CheckTimeout()) \
                            return false; \
                    } \
                    reader_.set_cursor(ll_code + offset); \
                } \
                break; \
            }

#define BINARY_JUMP_OP(op, oper) \
            case op: { \
                uint16_t a = reader_.read<uint16_t>(); \
                uint16_t b = reader_.read<uint16_t>(); \
                cell_t offset = reader_.read<int32_t>(); \
                if (vregs_[a] oper vregs_[b]) { \
                    if (offset < (cell_t)(insn_begin - ll_code)) { \
                        if (!CheckTimeout()) \
                            return false; \
                    } \
                    reader_.set_cursor(ll_code + offset); \
                } \
                break; \
            }

#include "watchdog_timer.h"

namespace sp::v2 {

bool Interpreter::Run(Runtime* cx, RefPtr<MethodInfo> method, cell_t* rval) {
    if (!method->interp()) {
        ke::RefPtr<ControlFlowGraph> graph = method->BuildGraph();
        if (!graph)
            return false;
        std::unique_ptr<InterpCode> code = LowerMethod(graph, method.get());
        method->setInterpCode(std::move(code));
    }

    Interpreter interpreter(cx, method);
    if (!interpreter.run_internal({}))
        return false;

    *rval = interpreter.return_value();
    return true;
}

Interpreter::Interpreter(Runtime* cx, RefPtr<MethodInfo> method)
 : env_(Environment::get()),
   rt_(cx),
   smx_(rt_->image()),
   heap_(rt_->heap()),
   method_(std::move(method)),
   code_(rt_->code().bytes),
   reader_(method_->interp()->bytes(), method_->interp()->bytes() + method_->interp()->size()),
   has_returned_(false),
   return_value_(0),
   frm_(env_->sp()),
   phys_frm_(cx->heap().ToPhysAddr<cell_t*>(frm_))
{}

bool Interpreter::CheckTimeout() {
    if (!env_->watchdog()->HandleInterrupt()) {
        rt_->ReportErrorNumber(SP_ERROR_TIMEOUT);
        return false;
    }
    return true;
}

bool Interpreter::run_internal(std::span<cell_t> args) {
    const uint8_t* insn_begin = reader_.cursor();
    const uint8_t* ll_code = method_->interp()->bytes();

    InterpInvokeFrame ivk(rt_, method_, &insn_begin);
    ke::SaveAndSet<InterpInvokeFrame*> enterIvk(&ivk_, &ivk);

    ke::ScopeGuard guard([&]() -> void {
        while (env_->top() != &ivk) {
            InvokeFrame* top = env_->top();
            top->AsInterpInvokeFrame()->~InterpInvokeFrame();
        }
    });

    ke::SaveRestore<uint32_t> saveSp(env_->sp());

    const smx_rtti_method* rtti = smx_->GetMethod(method_->method_index());
    bool is_global_ctor = (rtti->flags & kRttiMethod_GlobalCtor) != 0;

    std::optional<ke::SaveRestore<uint32_t>> saveHpScope;
    std::optional<HeapSave> saveHp;
    if (!is_global_ctor) {
        saveHpScope.emplace(rt_->hp_scope());
        saveHp.emplace(rt_->heap());
    }

    uint32_t num_regs = method_->interp()->num_regs();
    if (num_regs > 0) {
        uint32_t bytes = num_regs * sizeof(cell_t);
        if (!env_->addStack(-(cell_t)bytes))
            return false;
        vregs_ = std::span<cell_t>(rt_->heap().ToPhysAddr<cell_t*>(env_->sp()), num_regs);
    }

    uint32_t current_reg = 0;

    if (args.data() != nullptr) {
        for (size_t i = 0; i < method_->arg_types().size(); i++) {
            vregs_[current_reg++] = args[i];
        }
    } else {
        for (size_t i = 0; i < method_->arg_types().size(); i++) {
            vregs_[current_reg++] = phys_frm_[1 + i];
        }
    }

    uint32_t local_regs_start = current_reg;
    uint32_t num_local_regs = 0;
    for (size_t i = 0; i < method_->local_types().size(); i++) {
        num_local_regs += method_->local_types()[i]->slot_size() / sizeof(cell_t);
    }
    if (num_local_regs > 0)
        memset(&vregs_[local_regs_start], 0, num_local_regs * sizeof(cell_t));

    while (!has_returned_ && reader_.more()) {
        insn_begin = reader_.cursor();

        if (env_->IsDebugBreakEnabled()) {
            uint32_t ll_offset = (uint32_t)(insn_begin - ll_code);
            uint32_t high_offset = method_->interp()->LookupHighOffset(ll_offset);
            if (smx_->IsLineBoundary(high_offset)) {
                InvokeDebugger(rt_, nullptr);
                if (env_->hasPendingException())
                    return false;
            }
        }

#ifndef NDEBUG
        if (env_->spew_interp_ops()) {
            LLOp op = (LLOp)*reinterpret_cast<const uint16_t*>(insn_begin);
            fprintf(stdout, "  [%05u] %s\n", (uint32_t)(insn_begin - ll_code), GetLLOpName(op));
        }
#endif

        LLOp op = (LLOp)reader_.read<uint16_t>();

        switch (op) {
            case LL_NOP:
                break;
            case LL_LOAD_GLB_X32: {
                uint16_t index = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                cell_t addr = rt_->GetGlobalAddr(index);
                cell_t* ptr = heap_.ToPhysAddr<cell_t*>(addr);
                if (!ptr)
                    return false;
                vregs_[dest] = *ptr;
                break;
            }
            case LL_LOAD_GLB_X64: {
                uint16_t index = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                cell_t addr = rt_->GetGlobalAddr(index);
                int64_t* ptr = heap_.ToPhysAddr<int64_t*>(addr);
                if (!ptr)
                    return false;
                *reinterpret_cast<int64_t*>(&vregs_[dest]) = *ptr;
                break;
            }
            case LL_LOAD_I_I32:
            case LL_LOAD_I_F32: {
                uint16_t addr = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                cell_t* ptr = heap_.ToPhysAddr<cell_t*>(vregs_[addr]);
                if (!ptr)
                    return false;
                vregs_[dest] = *ptr;
                break;
            }
            case LL_LOAD_I_I64: {
                uint16_t addr = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                int64_t* ptr = heap_.ToPhysAddr<int64_t*>(vregs_[addr]);
                if (!ptr)
                    return false;
                *reinterpret_cast<int64_t*>(&vregs_[dest]) = *ptr;
                break;
            }
            case LL_LOAD_I_U8: {
                uint16_t addr = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                cell_t* ptr = heap_.ToPhysAddr<cell_t*>(vregs_[addr]);
                if (!ptr)
                    return false;
                vregs_[dest] = *ptr & 0xff;
                break;
            }
            case LL_LOAD_ELEM_A:
            case LL_LOAD_ELEM_I32:
            case LL_LOAD_ELEM_F32:
            case LL_LOAD_ELEM_I64:
            case LL_LOAD_ELEM_U8: {
                uint16_t base_reg = reader_.read<uint16_t>();
                uint16_t index_reg = reader_.read<uint16_t>();
                uint16_t dest_reg = reader_.read<uint16_t>();
                
                uint32_t base = vregs_[base_reg];
                uint32_t index = vregs_[index_reg];
                auto array = rt_->heap().ToPhysAddr<SpArray*>(base);
                if (index >= array->length) {
                    ReportOutOfBoundsError(index, array->length);
                    return false;
                }
                void* elt = rt_->GetArrayElem(array, index);
                if (op == LL_LOAD_ELEM_I64)
                    *reinterpret_cast<int64_t*>(&vregs_[dest_reg]) = *reinterpret_cast<int64_t*>(elt);
                else if (op == LL_LOAD_ELEM_U8)
                    vregs_[dest_reg] = *reinterpret_cast<uint8_t*>(elt);
                else
                    vregs_[dest_reg] = *reinterpret_cast<cell_t*>(elt);
                break;
            }
            case LL_ADDR_GLB: {
                uint16_t index = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = rt_->GetGlobalAddr(index);
                break;
            }
            case LL_STOR_GLB_X32: {
                uint16_t index = reader_.read<uint16_t>();
                uint16_t val = reader_.read<uint16_t>();
                cell_t addr = rt_->GetGlobalAddr(index);
                cell_t* ptr = heap_.ToPhysAddr<cell_t*>(addr);
                if (!ptr)
                    return false;
                *ptr = vregs_[val];
                break;
            }
            case LL_STOR_GLB_X64: {
                uint16_t index = reader_.read<uint16_t>();
                uint16_t val = reader_.read<uint16_t>();
                cell_t addr = rt_->GetGlobalAddr(index);
                int64_t* ptr = heap_.ToPhysAddr<int64_t*>(addr);
                if (!ptr)
                    return false;
                *ptr = *reinterpret_cast<int64_t*>(&vregs_[val]);
                break;
            }
            case LL_STOR_I_I32:
            case LL_STOR_I_F32: {
                uint16_t addr = reader_.read<uint16_t>();
                uint16_t val = reader_.read<uint16_t>();
                cell_t* ptr = heap_.ToPhysAddr<cell_t*>(vregs_[addr]);
                if (!ptr)
                    return false;
                *ptr = vregs_[val];
                break;
            }
            case LL_STOR_I_I64: {
                uint16_t addr = reader_.read<uint16_t>();
                uint16_t val = reader_.read<uint16_t>();
                int64_t* ptr = heap_.ToPhysAddr<int64_t*>(vregs_[addr]);
                if (!ptr)
                    return false;
                *ptr = *reinterpret_cast<int64_t*>(&vregs_[val]);
                break;
            }
            case LL_STOR_I_U8: {
                uint16_t addr = reader_.read<uint16_t>();
                uint16_t val = reader_.read<uint16_t>();
                uint8_t* dest_addr = rt_->heap().ToPhysAddr<uint8_t*>(vregs_[addr]);
                if (!dest_addr)
                    return false;
                *dest_addr = uint8_t(vregs_[val]);
                break;
            }
            case LL_MOVE: {
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = vregs_[src];
                break;
            }
            case LL_MOVE_I64: {
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                *reinterpret_cast<int64_t*>(&vregs_[dest]) = *reinterpret_cast<int64_t*>(&vregs_[src]);
                break;
            }
            case LL_LOAD_CONST: {
                cell_t val = reader_.read<cell_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = val;
                break;
            }
            case LL_LOAD_CONST_I64: {
                int64_t val = reader_.read<int64_t>();
                uint16_t dest = reader_.read<uint16_t>();
                *reinterpret_cast<int64_t*>(&vregs_[dest]) = val;
                break;
            }
            case LL_STOR_ELEM_I32:
            case LL_STOR_ELEM_F32:
            case LL_STOR_ELEM_I64:
            case LL_STOR_ELEM_U8: {
                uint16_t base_reg = reader_.read<uint16_t>();
                uint16_t index_reg = reader_.read<uint16_t>();
                uint16_t val_reg = reader_.read<uint16_t>();
                
                uint32_t base = vregs_[base_reg];
                uint32_t index = vregs_[index_reg];
                auto array = rt_->heap().ToPhysAddr<SpArray*>(base);
                if (index >= array->length) {
                    ReportOutOfBoundsError(index, array->length);
                    return false;
                }
                void* elt = rt_->GetArrayElem(array, index);
                if (op == LL_STOR_ELEM_I64)
                    *reinterpret_cast<int64_t*>(elt) = *reinterpret_cast<int64_t*>(&vregs_[val_reg]);
                else if (op == LL_STOR_ELEM_U8)
                    *reinterpret_cast<uint8_t*>(elt) = vregs_[val_reg] & 0xFF;
                else
                    *reinterpret_cast<cell_t*>(elt) = vregs_[val_reg];
                break;
            }
            case LL_IDXADDR: {
                uint16_t base_reg = reader_.read<uint16_t>();
                uint16_t index_reg = reader_.read<uint16_t>();
                uint16_t dest_reg = reader_.read<uint16_t>();
                
                uint32_t base = vregs_[base_reg];
                uint32_t index = vregs_[index_reg];
                auto array = rt_->heap().ToPhysAddr<SpArray*>(base);
                if (index >= array->length) {
                    ReportOutOfBoundsError(index, array->length);
                    return false;
                }
                void* elt_addr = rt_->GetArrayElem(array, index);
                vregs_[dest_reg] = rt_->heap().ToLocalAddr(elt_addr);
                break;
            }
            case LL_SLICE: {
                uint16_t base_reg = reader_.read<uint16_t>();
                uint16_t index_reg = reader_.read<uint16_t>();
                uint16_t dest_reg = reader_.read<uint16_t>();
                
                uint32_t base = vregs_[base_reg];
                uint32_t index = vregs_[index_reg];
                auto array = rt_->heap().ToPhysAddr<SpArray*>(base);
                if (index >= array->length) {
                    ReportOutOfBoundsError(index, array->length);
                    return false;
                }
                auto slice = rt_->NewSlice(array, index);
                if (!slice)
                    return false;
                vregs_[dest_reg] = rt_->heap().ToLocalAddr(slice.release());
                break;
            }
            case LL_IDXADDR_FLAT: {
                uint32_t size = reader_.read<uint32_t>();
                uint32_t elt_size = reader_.read<uint16_t>();
                uint16_t base_reg = reader_.read<uint16_t>();
                uint16_t index_reg = reader_.read<uint16_t>();
                uint16_t dest_reg = reader_.read<uint16_t>();
                
                cell_t base = vregs_[base_reg];
                cell_t index = vregs_[index_reg];
                if (index < 0 || (uint32_t)index >= size) {
                    ReportOutOfBoundsError(index, size);
                    return false;
                }
                vregs_[dest_reg] = base + index * elt_size;
                break;
            }
            case LL_ARRAY_TO_FLAT: {
                uint16_t addr_reg = reader_.read<uint16_t>();
                uint16_t dest_reg = reader_.read<uint16_t>();
                cell_t addr = vregs_[addr_reg];
                auto array = rt_->heap().ToPhysAddr<SpArray*>(addr);
                vregs_[dest_reg] = array->data;
                break;
            }
            case LL_COPYARRAY_FLAT: {
                uint32_t bytes = reader_.read<uint32_t>();
                uint16_t src_reg = reader_.read<uint16_t>();
                uint16_t dest_reg = reader_.read<uint16_t>();
                cell_t src_addr = vregs_[src_reg];
                cell_t dest_addr = vregs_[dest_reg];
                uint8_t* dest = rt_->heap().ToPhysAddr<uint8_t*>(dest_addr);
                uint8_t* src = rt_->heap().ToPhysAddr<uint8_t*>(src_addr);
                memcpy(dest, src, bytes);
                break;
            }
            case LL_FILLARRAY_FLAT: {
                uint32_t data_offs = reader_.read<uint32_t>();
                const TypeDesc* td = reader_.read<const TypeDesc*>();
                uint16_t addr_reg = reader_.read<uint16_t>();
                cell_t local_addr = vregs_[addr_reg];
                rt_->FillFlatArray(local_addr, td, data_offs);
                break;
            }
            case LL_SLICE_FLAT: {
                const TypeDesc* td = reader_.read<const TypeDesc*>();
                uint16_t base_reg = reader_.read<uint16_t>();
                uint16_t index_reg = reader_.read<uint16_t>();
                uint16_t dest_reg = reader_.read<uint16_t>();
                cell_t base = vregs_[base_reg];
                uint32_t index = vregs_[index_reg];
                auto slice = rt_->NewFlatSlice(base, td, index);
                if (!slice)
                    return false;
                vregs_[dest_reg] = rt_->heap().ToLocalAddr(slice.release());
                break;
            }
            case LL_STOR_ELEM_FLAT_I32:
            case LL_STOR_ELEM_FLAT_F32:
            case LL_STOR_ELEM_FLAT_I64:
            case LL_STOR_ELEM_FLAT_U8: {
                uint32_t array_size = reader_.read<uint32_t>();
                uint32_t element_size = reader_.read<uint16_t>();
                uint16_t base_reg = reader_.read<uint16_t>();
                uint16_t index_reg = reader_.read<uint16_t>();
                uint16_t val_reg = reader_.read<uint16_t>();
                
                uint32_t base = vregs_[base_reg];
                uint32_t index = vregs_[index_reg];
                if (index >= array_size) {
                    ReportOutOfBoundsError(index, array_size);
                    return false;
                }
                uint8_t* elt = rt_->heap().ToPhysAddr<uint8_t*>(base + (index * element_size));
                if (op == LL_STOR_ELEM_FLAT_I64)
                    *reinterpret_cast<int64_t*>(elt) = *reinterpret_cast<int64_t*>(&vregs_[val_reg]);
                else if (op == LL_STOR_ELEM_FLAT_U8)
                    *reinterpret_cast<uint8_t*>(elt) = vregs_[val_reg] & 0xFF;
                else
                    *reinterpret_cast<cell_t*>(elt) = vregs_[val_reg];
                break;
            }

            BINARY_OP_I64(LL_SMUL_I64, *)
            case LL_SDIV_I64: {
                uint16_t a = reader_.read<uint16_t>();
                uint16_t b = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                int64_t left = *reinterpret_cast<int64_t*>(&vregs_[a]);
                int64_t right = *reinterpret_cast<int64_t*>(&vregs_[b]);
                int64_t result;
                int err = Int64Div(&right, &left, &result);
                if (err != SP_ERROR_NONE) {
                    rt_->ReportErrorNumber(err);
                    return false;
                }
                *reinterpret_cast<int64_t*>(&vregs_[dest]) = result;
                break;
            }
            case LL_SMOD_I64: {
                uint16_t a = reader_.read<uint16_t>();
                uint16_t b = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                int64_t left = *reinterpret_cast<int64_t*>(&vregs_[a]);
                int64_t right = *reinterpret_cast<int64_t*>(&vregs_[b]);
                int64_t result;
                int err = Int64Mod(&right, &left, &result);
                if (err != SP_ERROR_NONE) {
                    rt_->ReportErrorNumber(err);
                    return false;
                }
                *reinterpret_cast<int64_t*>(&vregs_[dest]) = result;
                break;
            }
            BINARY_OP_I64(LL_ADD_I64, +)
            BINARY_OP_I64(LL_SUB_I64, -)
            BINARY_OP_I64(LL_SHL_I64, <<)
            BINARY_OP_I64(LL_SSHR_I64, >>)
            case LL_SHR_I64: {
                uint16_t a = reader_.read<uint16_t>();
                uint16_t b = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                uint64_t left = *reinterpret_cast<uint64_t*>(&vregs_[a]);
                uint64_t right = *reinterpret_cast<uint64_t*>(&vregs_[b]);
                *reinterpret_cast<uint64_t*>(&vregs_[dest]) = (left >> right);
                break;
            }
            BINARY_OP_I64(LL_OR_I64, |)
            BINARY_OP_I64(LL_AND_I64, &)
            BINARY_OP_I64(LL_XOR_I64, ^)
            COMPARE_OP_I64(LL_EQ_I64, ==)
            COMPARE_OP_I64(LL_NEQ_I64, !=)
            COMPARE_OP_I64(LL_SLESS_I64, <)
            COMPARE_OP_I64(LL_SLEQ_I64, <=)
            COMPARE_OP_I64(LL_SGRTR_I64, >)
            COMPARE_OP_I64(LL_SGEQ_I64, >=)
            case LL_NEG_I64: {
                uint16_t a = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                *reinterpret_cast<int64_t*>(&vregs_[dest]) = -(*reinterpret_cast<int64_t*>(&vregs_[a]));
                break;
            }
            case LL_INVERT_I64: {
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                *reinterpret_cast<int64_t*>(&vregs_[dest]) =
                    ~(*reinterpret_cast<int64_t*>(&vregs_[src]));
                break;
            }
            case LL_TEST_I64: {
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = (*reinterpret_cast<int64_t*>(&vregs_[src]) != 0) ? 1 : 0;
                break;
            }
            case LL_RETN:
            case LL_RETV: {
                cell_t result = 0;
                if (op == LL_RETN) {
                    uint16_t srcreg = reader_.read<uint16_t>();
                    result = vregs_[srcreg];
                }

                if (ivk_ == &ivk) {
                    has_returned_ = true;
                    return_value_ = result;
                    break;
                }

                InterpFrame* frame = rt_->heap().ToPhysAddr<InterpFrame*>(frm_);
                const smx_rtti_method* rtti = smx_->GetMethod(method_->method_index());
                bool is_global_ctor = (rtti->flags & kRttiMethod_GlobalCtor) != 0;

                ivk_->~InterpInvokeFrame();

                ivk_ = env_->top()->AsInterpInvokeFrame();
                ivk_->setCip(&insn_begin);

                method_ = frame->caller_method;

                const uint8_t* caller_code = method_->interp()->bytes();
                reader_ = BinaryReader(caller_code, caller_code + method_->interp()->size());
                reader_.set_cursor(frame->saved_cip);
                ll_code = caller_code;

                size_t frame_size = ke::Align(sizeof(InterpFrame), alignof(std::max_align_t));
                cell_t stack_amount = (cell_t)(frm_ + frame_size - env_->sp());
                if (!env_->addStack(stack_amount))
                    return false;
                frm_ = frame->prev_frame;

                if (!is_global_ctor) {
                    rt_->hp_scope() = frame->hp_scope;
                    rt_->heap().RestorePosition(frame->heap_pos);
                }

                uint32_t num_caller_regs = method_->interp()->num_regs();
                vregs_ = std::span<cell_t>(rt_->heap().ToPhysAddr<cell_t*>(env_->sp()), num_caller_regs);

                if (frame->dest_reg != 0xFFFF)
                    vregs_[frame->dest_reg] = result;
                break;
            }
            case LL_LOAD_FN: {
                uint32_t method_index = reader_.read<uint32_t>();
                uint16_t dest = reader_.read<uint16_t>();
                funcid_t id = (method_index << 1) | 1;
                vregs_[dest] = id;
                break;
            }
            case LL_LOAD_STR: {
                uint32_t index = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = rt_->GetStringAddr(index);
                break;
            }
            case LL_LOAD_FLD_X32: {
                uint32_t offset = reader_.read<uint32_t>();
                uint16_t base = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                uint8_t* base_ptr = rt_->heap().ToPhysAddr<uint8_t*>(vregs_[base]);
                vregs_[dest] = *reinterpret_cast<cell_t*>(base_ptr + offset);
                break;
            }
            case LL_LOAD_FLD_X64: {
                uint32_t offset = reader_.read<uint32_t>();
                uint16_t base = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                uint8_t* base_ptr = rt_->heap().ToPhysAddr<uint8_t*>(vregs_[base]);
                *reinterpret_cast<int64_t*>(&vregs_[dest]) =
                    *reinterpret_cast<int64_t*>(base_ptr + offset);
                break;
            }
            case LL_ADDR_FLD: {
                uint32_t offset = reader_.read<uint32_t>();
                uint16_t base = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = vregs_[base] + offset;
                break;
            }
            case LL_STOR_FLD_X32: {
                uint32_t offset = reader_.read<uint32_t>();
                uint16_t basereg = reader_.read<uint16_t>();
                uint16_t valreg = reader_.read<uint16_t>();
                cell_t obj_addr = vregs_[basereg];
                uint8_t* base_ptr = rt_->heap().ToPhysAddr<uint8_t*>(obj_addr);
                *reinterpret_cast<cell_t*>(base_ptr + offset) = vregs_[valreg];
                break;
            }
            case LL_STOR_FLD_X64: {
                uint32_t offset = reader_.read<uint32_t>();
                uint16_t basereg = reader_.read<uint16_t>();
                uint16_t valreg = reader_.read<uint16_t>();
                cell_t obj_addr = vregs_[basereg];
                uint8_t* base_ptr = rt_->heap().ToPhysAddr<uint8_t*>(obj_addr);
                *reinterpret_cast<int64_t*>(base_ptr + offset) =
                    *reinterpret_cast<int64_t*>(&vregs_[valreg]);
                break;
            }
            case LL_CALL: {
                const smx_rtti_method* method = reader_.read<const smx_rtti_method*>();
                uint8_t nargs = reader_.read<uint8_t>();
                uint16_t dest = reader_.read<uint16_t>();

                uint32_t method_index = method - smx_->GetMethod(0);
                uint32_t native_index;
                cell_t result = 0;
                if (rt_->GetNativeIndex(method_index, &native_index)) {
                    cell_t params[256];
                    params[0] = nargs;
                    for (uint8_t i = 0; i < nargs; i++) {
                        uint16_t arg_reg = reader_.read<uint16_t>();
                        params[i + 1] = vregs_[arg_reg];
                    }

                    NativeEntry* native = rt_->NativeAt(native_index);
                    ivk_->enterNativeCall(native_index);
                    if (native->status == SP_NATIVE_BOUND) {
                        HeapSave save_hp(rt_->heap());
                        ke::SaveRestore<uint32_t> save_sp(env_->sp());

                        if (native->legacy_fn)
                            result = native->legacy_fn(rt_, params);
                        else
                            result = native->callback->Invoke(rt_, params);
                    } else {
                        rt_->ReportErrorNumber(SP_ERROR_INVALID_NATIVE);
                    }
                    ivk_->leaveNativeCall();
                    if (env_->hasPendingException())
                        return false;
                } else {
                    RefPtr<MethodInfo> target = rt_->AcquireMethod(method_index);
                    if (!target->Validate())
                        return false;
                    
                    if (!target->interp()) {
                        ke::RefPtr<ControlFlowGraph> graph = target->BuildGraph();
                        if (!graph) return false;
                        std::unique_ptr<InterpCode> code = LowerMethod(graph, target.get());
                        target->setInterpCode(std::move(code));
                    }

                    size_t frame_size = ke::Align(sizeof(InterpFrame), alignof(std::max_align_t));
                    uint32_t num_callee_regs = target->interp()->num_regs();
                    cell_t stack_amount = -(cell_t)(frame_size + num_callee_regs * sizeof(cell_t));
                    if (!env_->addStack(stack_amount))
                        return false;

                    uint32_t frame_addr = env_->sp() + num_callee_regs * sizeof(cell_t);

                    cell_t* new_vregs = rt_->heap().ToPhysAddr<cell_t*>(env_->sp());
                    for (uint8_t i = 0; i < nargs; i++) {
                        uint16_t arg_reg = reader_.read<uint16_t>();
                        new_vregs[i] = vregs_[arg_reg];
                    }

                    InterpFrame* frame = rt_->heap().ToPhysAddr<InterpFrame*>(frame_addr);
                    frame->caller_method = method_.get();
                    frame->saved_cip = reader_.cursor();
                    frame->dest_reg = dest;
                    frame->prev_frame = frm_;
                    frame->hp_scope = rt_->hp_scope();
                    frame->heap_pos = rt_->heap().GetPosition();

                    ivk_->setCip(&frame->saved_cip);

                    new (&frame->ivk) InterpInvokeFrame(rt_, target.get(), &insn_begin);

                    frm_ = frame_addr;
                    ivk_ = &frame->ivk;
                    vregs_ = std::span<cell_t>(new_vregs, num_callee_regs);
                    method_ = target;

                    const uint8_t* callee_code = method_->interp()->bytes();
                    reader_ = BinaryReader(callee_code, callee_code + method_->interp()->size());
                    ll_code = callee_code;
                    continue;
                }

                if (dest != 0xFFFF)
                    vregs_[dest] = result;
                break;
            }
            case LL_JUMP: {
                cell_t offset = reader_.read<int32_t>();
                if (offset < (cell_t)(insn_begin - ll_code)) {
                    if (!CheckTimeout())
                        return false;
                }
                reader_.set_cursor(ll_code + offset);
                break;
            }
            UNARY_JUMP_OP(LL_JZER, ==)
            UNARY_JUMP_OP(LL_JNZ, !=)
            BINARY_JUMP_OP(LL_JEQ, ==)
            BINARY_JUMP_OP(LL_JNEQ, !=)
            BINARY_JUMP_OP(LL_JSLESS, <)
            BINARY_JUMP_OP(LL_JSLEQ, <=)
            BINARY_JUMP_OP(LL_JSGRTR, >)
            BINARY_JUMP_OP(LL_JSGEQ, >=)
            case LL_SWITCH: {
                uint16_t val_reg = reader_.read<uint16_t>();
                cell_t ncases = reader_.read<cell_t>();
                cell_t default_offset = reader_.read<int32_t>();
                cell_t val = vregs_[val_reg];
                cell_t jump_offset = default_offset;
                for (cell_t i = 0; i < ncases; i++) {
                    cell_t case_val = reader_.read<cell_t>();
                    cell_t case_offset = reader_.read<int32_t>();
                    if (case_val == val)
                        jump_offset = case_offset;
                }
                reader_.set_cursor(ll_code + jump_offset);
                break;
            }
            BINARY_OP_I32(LL_SHL, <<)
            BINARY_OP_I32(LL_SSHR, >>)
            case LL_SHR: {
                uint16_t a = reader_.read<uint16_t>();
                uint16_t b = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = uint32_t(vregs_[a]) >> uint32_t(vregs_[b]);
                break;
            }
            BINARY_OP_I32(LL_SMUL_I32, *)
            case LL_SDIV_I32: {
                uint16_t a = reader_.read<uint16_t>();
                uint16_t b = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                if (vregs_[b] == 0) {
                    rt_->ReportErrorNumber(SP_ERROR_DIVIDE_BY_ZERO);
                    return false;
                }
                if (vregs_[b] == -1 && vregs_[a] == cell_t(0x80000000)) {
                    rt_->ReportErrorNumber(SP_ERROR_INTEGER_OVERFLOW);
                    return false;
                }
                vregs_[dest] = vregs_[a] / vregs_[b];
                break;
            }
            case LL_SMOD_I32: {
                uint16_t a = reader_.read<uint16_t>();
                uint16_t b = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                if (vregs_[b] == 0) {
                    rt_->ReportErrorNumber(SP_ERROR_DIVIDE_BY_ZERO);
                    return false;
                }
                if (vregs_[b] == -1 && vregs_[a] == cell_t(0x80000000)) {
                    rt_->ReportErrorNumber(SP_ERROR_INTEGER_OVERFLOW);
                    return false;
                }
                vregs_[dest] = vregs_[a] % vregs_[b];
                break;
            }
            BINARY_OP_I32(LL_ADD_I32, +)
            BINARY_OP_I32(LL_SUB_I32, -)
            BINARY_OP_I32(LL_AND, &)
            BINARY_OP_I32(LL_OR, |)
            BINARY_OP_I32(LL_XOR, ^)
            BINARY_OP_I32(LL_EQ_I32, ==)
            BINARY_OP_I32(LL_NEQ_I32, !=)
            BINARY_OP_I32(LL_SLESS_I32, <)
            BINARY_OP_I32(LL_SLEQ_I32, <=)
            BINARY_OP_I32(LL_SGRTR_I32, >)
            BINARY_OP_I32(LL_SGEQ_I32, >=)
            case LL_NOT: {
                uint16_t a = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = vregs_[a] ? 0 : 1;
                break;
            }
            case LL_NEG: {
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = -vregs_[src];
                break;
            }
            case LL_INVERT: {
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = ~vregs_[src];
                break;
            }
            case LL_TEST_I32: {
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = vregs_[src] ? 1 : 0;
                break;
            }
            case LL_TEST_F32: {
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                float f = sp_ctof(vregs_[src]);
                vregs_[dest] = (f != 0.0f && !ke::IsNaN(f)) ? 1 : 0;
                break;
            }
            case LL_NEG_F32: {
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = sp_ftoc(-sp_ctof(vregs_[src]));
                break;
            }
            BINARY_OP_F32(LL_MUL_F32, *)
            BINARY_OP_F32(LL_DIV_F32, /)
            case LL_MOD_F32: {
                uint16_t a = reader_.read<uint16_t>();
                uint16_t b = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = sp_ftoc(fmodf(sp_ctof(vregs_[a]), sp_ctof(vregs_[b])));
                break;
            }
            BINARY_OP_F32(LL_ADD_F32, +)
            BINARY_OP_F32(LL_SUB_F32, -)
            COMPARE_OP_F32(LL_EQ_F32, ==)
            COMPARE_OP_F32(LL_NEQ_F32, !=)
            COMPARE_OP_F32(LL_LESS_F32, <)
            COMPARE_OP_F32(LL_LEQ_F32, <=)
            COMPARE_OP_F32(LL_GRTR_F32, >)
            COMPARE_OP_F32(LL_GEQ_F32, >=)
            case LL_CVT_F32: {
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = sp_ftoc((float)vregs_[src]);
                break;
            }
            case LL_CVT_I64: {
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                *reinterpret_cast<int64_t*>(&vregs_[dest]) = (int64_t)vregs_[src];
                break;
            }
            case LL_TRUNCATE_I64: {
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = (cell_t)*reinterpret_cast<int64_t*>(&vregs_[src]);
                break;
            }
            case LL_COPYARRAY: {
                uint16_t srcreg = reader_.read<uint16_t>(); // src
                uint16_t destreg = reader_.read<uint16_t>(); // dest
                SpArray* src = heap_.ToPhysAddr<SpArray*>(vregs_[srcreg]);
                SpArray* dest = heap_.ToPhysAddr<SpArray*>(vregs_[destreg]);
                assert(dest->td->kind() == TypeKind::FixedArray);

                if (src->length > dest->length) {
                    rt_->ReportErrorNumber(SP_ERROR_ARRAY_BOUNDS);
                    return false;
                }

                [[maybe_unused]] auto src_elt = src->td->array_elt();
                [[maybe_unused]] auto dest_elt = dest->td->array_elt();
                assert(src_elt->element_size() == dest_elt->element_size());

                auto src_data = heap_.ToPhysAddr<uint8_t*>(src->data);
                auto dest_data = heap_.ToPhysAddr<uint8_t*>(dest->data);
                memcpy(dest_data, src_data, src->length * src_elt->element_size());
                break;
            }
            case LL_COPYOBJ: {
                uint32_t bytes = reader_.read<uint32_t>();
                uint16_t srcreg = reader_.read<uint16_t>();
                uint16_t destreg = reader_.read<uint16_t>();
                cell_t src_addr = vregs_[srcreg];
                cell_t dest_addr = vregs_[destreg];
                uint8_t* dest = heap_.ToPhysAddr<uint8_t*>(dest_addr);
                uint8_t* src = heap_.ToPhysAddr<uint8_t*>(src_addr);
                memcpy(dest, src, bytes);
                break;
            }
            case LL_NEWARRAY: {
                auto td = reader_.read<const TypeDesc*>();
                uint16_t size_reg = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                uint32_t size;
                if (td->kind() == TypeKind::Array) {
                    cell_t val = vregs_[size_reg];
                    if (val < 0) {
                        rt_->ReportErrorNumber(SP_ERROR_ARRAY_BOUNDS);
                        return false;
                    }
                    size = val;
                } else {
                    size = td->array_size();
                }
                auto array = rt_->NewArray(td, size);
                if (!array)
                    return false;
                vregs_[dest] = rt_->heap().ToLocalAddr(array.release());
                break;
            }

            case LL_NEWFIXEDARRAY: {
                auto td = reader_.read<const TypeDesc*>();
                uint16_t dest = reader_.read<uint16_t>();
                uint32_t size = td->array_size();
                auto array = rt_->NewArray(td, size);
                if (!array)
                    return false;
                vregs_[dest] = rt_->heap().ToLocalAddr(array.release());
                break;
            }
            case LL_NEWBULKARRAY: {
                uint8_t dims = reader_.read<uint8_t>();
                auto td = reader_.read<const TypeDesc*>();
                uint16_t size_reg = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();

                auto array = rt_->NewBulkArray(td, dims, &vregs_[size_reg]);

                if (!array)
                    return false;
                vregs_[dest] = rt_->heap().ToLocalAddr(array.release());
                break;
            }
            case LL_FILLARRAY: {
                uint32_t data_offset = reader_.read<uint32_t>();
                uint16_t src = reader_.read<uint16_t>(); // array addr
                auto array = rt_->heap().ToPhysAddr<SpArray*>(vregs_[src]);
                if (!rt_->FillArray(array, data_offset))
                    return false;
                break;
            }
            case LL_ARRAY_TO_NATIVE: {
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                uint32_t addr = vregs_[src];
                assert((addr & kNativePointerTag) == 0);
                vregs_[dest] = addr | kNativePointerTag;
                break;
            }
            case LL_SLICE_ES: {
                uint32_t cell_count = reader_.read<uint32_t>();
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                cell_t base = vregs_[src];
                SpArray* slice = rt_->heap().AllocTyped<SpArray>();
                if (!slice)
                    return false;
                slice->td = rt_->GetSliceType(rt_->GetPrimitiveType(TypeKind::Any));
                slice->length = cell_count;
                slice->data = base;
                vregs_[dest] = rt_->heap().ToLocalAddr(slice);
                break;
            }
            case LL_HEAP_SAVE: {
                if (!rt_->enterHeapScope())
                    return false;
                break;
            }
            case LL_HEAP_RESTORE: {
                rt_->leaveHeapScope();
                break;
            }
            case LL_ADDR_S: {
                uint16_t reg = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = rt_->heap().ToLocalAddr(&vregs_[reg]);
                break;
            }

            default:
                fprintf(stderr, "Unimplemented opcode: %s\n", GetLLOpName(op));
                assert(false);
                return false;
        }
    }

    return true;
}



} // namespace sp::v2
