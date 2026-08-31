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
#include "v2/interpreter.h"

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
#include "v2/lowering/llcode.h"
#include "v2/lowering/ll-op.h"
#include "v2/lowering/lowering.h"
#include "v2/method-info.h"
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

bool Interpreter::Run(Runtime* cx, Handle<SpFunction> fn, uint32_t frm, cell_t* rval) {
    auto* method = fn->method;
    if (!method->llcode()) {
        ke::RefPtr<ControlFlowGraph> graph = method->BuildGraph();
        if (!graph)
            return false;
        std::unique_ptr<LLCode> code = LowerMethod(graph, method);
        method->set_llcode(std::move(code));
    }

    Interpreter interpreter(cx, std::move(fn), frm);
    if (!interpreter.run_internal())
        return false;

    *rval = interpreter.return_value();
    return true;
}

Interpreter::Interpreter(Runtime* cx, Handle<SpFunction> fn, uint32_t frm)
  : env_(Environment::get()),
    rt_(cx),
    smx_(rt_->image()),
    heap_(rt_->heap()),
    code_(rt_->code().bytes),
    reader_(fn->method->llcode()->bytes(), fn->method->llcode()->bytes() + fn->method->llcode()->size()),
    has_returned_(false),
    return_value_(0),
    frm_(frm),
    phys_frm_(cx->heap().ToPhysAddr<cell_t*>(frm_)),
    entry_fn_(std::move(fn))
{}

bool Interpreter::CheckTimeout() {
    if (!env_->watchdog()->HandleInterrupt()) {
        rt_->ReportErrorNumber(SP_ERROR_TIMEOUT);
        return false;
    }
    return true;
}

bool Interpreter::run_internal() {
    const uint8_t* insn_begin = reader_.cursor();
    const uint8_t* ll_code = entry_fn_->method->llcode()->bytes();

    InterpFrame ivk(entry_fn_.get(), rt_, &insn_begin, nullptr, 0, 0);
    ke::SaveAndSet<InterpFrame*> enterIvk(&ivk_, &ivk);

    std::span<cell_t> root_vregs;

    ke::ScopeGuard guard([this, &ivk, &root_vregs]() -> void {
        UnwindStack(&ivk, entry_fn_->method, root_vregs);
    });

    ke::SaveRestore<uint32_t> saveSp(env_->sp());

    uint32_t num_params = entry_fn_->method->arg_types().size();
    uint32_t num_regs = entry_fn_->method->llcode()->num_regs();
    uint32_t callee_regs = num_regs - num_params;

    if (callee_regs > 0) {
        if (!env_->addStack(callee_regs * sizeof(cell_t)))
            return false;
    }
    if (num_regs > 0) {
        vregs_ = std::span<cell_t>(rt_->heap().ToPhysAddr<cell_t*>(frm_), num_regs);
        root_vregs = vregs_;
        if (callee_regs > 0)
            memset(&vregs_[num_params], 0, callee_regs * sizeof(cell_t));
    }

    while (!has_returned_ && reader_.more()) {
        insn_begin = reader_.cursor();

        if (env_->IsDebugBreakEnabled()) {
            auto method = ivk_->callee()->method;
            uint32_t ll_offset = (uint32_t)(insn_begin - ll_code);
            uint32_t high_offset = method->llcode()->LookupHighOffset(ll_offset);
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
                vregs_[dest] = *ptr;
                break;
            }
            case LL_LOAD_GLB_X64: {
                uint16_t index = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                cell_t addr = rt_->GetGlobalAddr(index);
                int64_t* ptr = heap_.ToPhysAddr<int64_t*>(addr);
                *reinterpret_cast<int64_t*>(&vregs_[dest]) = *ptr;
                break;
            }
            case LL_LOAD_GLB_A: {
                uint16_t index = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                cell_t addr = rt_->GetGlobalAddr(index);
                cell_t* ptr = heap_.ToPhysAddr<cell_t*>(addr);
                vregs_[dest] = *ptr;
                if (vregs_[dest])
                    heap_.ToPhysAddr<HeapItem*>(vregs_[dest])->AddRef();
                break;
            }
            case LL_LOAD_I_I32:
            case LL_LOAD_I_F32: {
                uint16_t addr = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                cell_t* ptr = heap_.ToPhysAddr<cell_t*>(vregs_[addr]);
                if (!ptr) {
                    rt_->ReportErrorNumber(SP_ERROR_NULL_DEREF);
                    return false;
                }
                vregs_[dest] = *ptr;
                break;
            }
            case LL_LOAD_I_I64: {
                uint16_t addr = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                int64_t* ptr = heap_.ToPhysAddr<int64_t*>(vregs_[addr]);
                if (!ptr) {
                    rt_->ReportErrorNumber(SP_ERROR_NULL_DEREF);
                    return false;
                }
                *reinterpret_cast<int64_t*>(&vregs_[dest]) = *ptr;
                break;
            }
            case LL_LOAD_I_U8: {
                uint16_t addr = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                cell_t* ptr = heap_.ToPhysAddr<cell_t*>(vregs_[addr]);
                if (!ptr) {
                    rt_->ReportErrorNumber(SP_ERROR_NULL_DEREF);
                    return false;
                }
                vregs_[dest] = *ptr & 0xff;
                break;
            }
            case LL_LOAD_I_I16: {
                uint16_t addr = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                cell_t* ptr = heap_.ToPhysAddr<cell_t*>(vregs_[addr]);
                if (!ptr) {
                    rt_->ReportErrorNumber(SP_ERROR_NULL_DEREF);
                    return false;
                }
                vregs_[dest] = *reinterpret_cast<int16_t*>(ptr);
                break;
            }
            case LL_LOAD_ELEM_A:
            case LL_LOAD_ELEM_I32:
            case LL_LOAD_ELEM_F32:
            case LL_LOAD_ELEM_I64:
            case LL_LOAD_ELEM_U8:
            case LL_LOAD_ELEM_I16: {
                uint16_t base_reg = reader_.read<uint16_t>();
                uint16_t index_reg = reader_.read<uint16_t>();
                uint16_t dest_reg = reader_.read<uint16_t>();

                uint32_t base = vregs_[base_reg];
                uint32_t index = vregs_[index_reg];
                auto array = rt_->heap().ToPhysAddr<SpArray*>(base);
                if (!array) {
                    rt_->ReportErrorNumber(SP_ERROR_NULL_DEREF);
                    return false;
                }
                if (index >= array->length) {
                    ReportOutOfBoundsError(index, array->length);
                    return false;
                }
                void* elt = rt_->GetArrayElem(array, index);
                if (op == LL_LOAD_ELEM_A) {
                    cell_t obj = *reinterpret_cast<cell_t*>(elt);
                    vregs_[dest_reg] = obj;
                    if (obj)
                        heap_.ToPhysAddr<HeapItem*>(obj)->AddRef();
                } else if (op == LL_LOAD_ELEM_I64) {
                    *reinterpret_cast<int64_t*>(&vregs_[dest_reg]) = *reinterpret_cast<int64_t*>(elt);
                } else if (op == LL_LOAD_ELEM_U8) {
                    vregs_[dest_reg] = *reinterpret_cast<uint8_t*>(elt);
                } else if (op == LL_LOAD_ELEM_I16) {
                    vregs_[dest_reg] = *reinterpret_cast<int16_t*>(elt);
                } else {
                    vregs_[dest_reg] = *reinterpret_cast<cell_t*>(elt);
                }
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
                *ptr = vregs_[val];
                break;
            }
            case LL_STOR_GLB_X64: {
                uint16_t index = reader_.read<uint16_t>();
                uint16_t val = reader_.read<uint16_t>();
                cell_t addr = rt_->GetGlobalAddr(index);
                int64_t* ptr = heap_.ToPhysAddr<int64_t*>(addr);
                *ptr = *reinterpret_cast<int64_t*>(&vregs_[val]);
                break;
            }
            case LL_STOR_GLB_A: {
                uint16_t index_reg = reader_.read<uint16_t>();
                uint16_t val_reg = reader_.read<uint16_t>();
                cell_t addr = rt_->GetGlobalAddr(index_reg);
                cell_t* ptr = heap_.ToPhysAddr<cell_t*>(addr);
                auto new_item = heap_.ToPhysAddr<HeapItem*>(vregs_[val_reg]);
                if (new_item && new_item->td->kind() == TypeKind::ArraySlice) {
                    rt_->ReportErrorNumber(SP_ERROR_SLICE_ESCAPE);
                    return false;
                }
                if (auto old_item = heap_.ToPhysAddr<HeapItem*>(*ptr))
                    old_item->Release();
                if (new_item)
                    new_item->AddRef();
                *ptr = vregs_[val_reg];
                break;
            }
            case LL_STOR_I_I32:
            case LL_STOR_I_F32: {
                uint16_t addr = reader_.read<uint16_t>();
                uint16_t val = reader_.read<uint16_t>();
                cell_t* ptr = heap_.ToPhysAddr<cell_t*>(vregs_[addr]);
                if (!ptr) {
                    rt_->ReportErrorNumber(SP_ERROR_NULL_DEREF);
                    return false;
                }
                *ptr = vregs_[val];
                break;
            }
            case LL_STOR_I_I64: {
                uint16_t addr = reader_.read<uint16_t>();
                uint16_t val = reader_.read<uint16_t>();
                int64_t* ptr = heap_.ToPhysAddr<int64_t*>(vregs_[addr]);
                if (!ptr) {
                    rt_->ReportErrorNumber(SP_ERROR_NULL_DEREF);
                    return false;
                }
                *ptr = *reinterpret_cast<int64_t*>(&vregs_[val]);
                break;
            }
            case LL_STOR_I_U8: {
                uint16_t addr = reader_.read<uint16_t>();
                uint16_t val = reader_.read<uint16_t>();
                uint8_t* dest_addr = rt_->heap().ToPhysAddr<uint8_t*>(vregs_[addr]);
                if (!dest_addr) {
                    rt_->ReportErrorNumber(SP_ERROR_NULL_DEREF);
                    return false;
                }
                *dest_addr = uint8_t(vregs_[val]);
                break;
            }
            case LL_STOR_I_I16: {
                uint16_t addr = reader_.read<uint16_t>();
                uint16_t val = reader_.read<uint16_t>();
                int16_t* dest_addr = rt_->heap().ToPhysAddr<int16_t*>(vregs_[addr]);
                if (!dest_addr) {
                    rt_->ReportErrorNumber(SP_ERROR_NULL_DEREF);
                    return false;
                }
                *dest_addr = int16_t(vregs_[val] & 0xffff);
                break;
            }
            case LL_STOR_I_A: {
                uint16_t addr_reg = reader_.read<uint16_t>();
                uint16_t val_reg = reader_.read<uint16_t>();
                cell_t* ptr = heap_.ToPhysAddr<cell_t*>(vregs_[addr_reg]);
                if (!ptr) {
                    rt_->ReportErrorNumber(SP_ERROR_NULL_DEREF);
                    return false;
                }
                auto new_item = heap_.ToPhysAddr<HeapItem*>(vregs_[val_reg]);
                if (new_item && new_item->td->kind() == TypeKind::ArraySlice) {
                    rt_->ReportErrorNumber(SP_ERROR_SLICE_ESCAPE);
                    return false;
                }
                if (auto old_item = heap_.ToPhysAddr<HeapItem*>(*ptr))
                    old_item->Release();
                if (new_item)
                    new_item->AddRef();
                *ptr = vregs_[val_reg];
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
            case LL_STOR_ELEM_U8:
            case LL_STOR_ELEM_I16:
            case LL_STOR_ELEM_A: {
                uint16_t base_reg = reader_.read<uint16_t>();
                uint16_t index_reg = reader_.read<uint16_t>();
                uint16_t val_reg = reader_.read<uint16_t>();

                uint32_t base = vregs_[base_reg];
                uint32_t index = vregs_[index_reg];
                auto array = rt_->heap().ToPhysAddr<SpArray*>(base);
                if (!array) {
                    rt_->ReportErrorNumber(SP_ERROR_NULL_DEREF);
                    return false;
                }
                if (index >= array->length) {
                    ReportOutOfBoundsError(index, array->length);
                    return false;
                }
                void* elt = rt_->GetArrayElem(array, index);
                if (op == LL_STOR_ELEM_I64) {
                    *reinterpret_cast<int64_t*>(elt) = *reinterpret_cast<int64_t*>(&vregs_[val_reg]);
                } else if (op == LL_STOR_ELEM_U8) {
                    *reinterpret_cast<uint8_t*>(elt) = vregs_[val_reg] & 0xFF;
                } else if (op == LL_STOR_ELEM_I16) {
                    *reinterpret_cast<int16_t*>(elt) = int16_t(vregs_[val_reg] & 0xffff);
                } else if (op == LL_STOR_ELEM_A) {
                    cell_t* ptr = reinterpret_cast<cell_t*>(elt);
                    auto new_item = heap_.ToPhysAddr<HeapItem*>(vregs_[val_reg]);
                    if (new_item && new_item->td->kind() == TypeKind::ArraySlice) {
                        rt_->ReportErrorNumber(SP_ERROR_SLICE_ESCAPE);
                        return false;
                    }
                    if (auto old_item = heap_.ToPhysAddr<HeapItem*>(*ptr))
                        old_item->Release();
                    if (new_item)
                        new_item->AddRef();
                    *ptr = vregs_[val_reg];
                } else {
                    *reinterpret_cast<cell_t*>(elt) = vregs_[val_reg];
                }
                break;
            }
            case LL_IDXADDR: {
                auto args = reader_.read<IdxAddrArgs>();

                uint32_t base = vregs_[args.base_reg];
                uint32_t index = vregs_[args.index_reg];
                auto array = rt_->heap().ToPhysAddr<SpArray*>(base);
                if (!array) {
                    rt_->ReportErrorNumber(SP_ERROR_NULL_DEREF);
                    return false;
                }
                if (index >= array->length) {
                    ReportOutOfBoundsError(index, array->length);
                    return false;
                }
                vregs_[args.dest_reg] = array->data + args.elt_size * index;
                break;
            }
            case LL_SLICE: {
                uint16_t base_reg = reader_.read<uint16_t>();
                uint16_t index_reg = reader_.read<uint16_t>();
                uint16_t dest_reg = reader_.read<uint16_t>();

                uint32_t base = vregs_[base_reg];
                uint32_t index = vregs_[index_reg];
                auto array = rt_->heap().ToPhysAddr<SpArray*>(base);
                if (!array) {
                    rt_->ReportErrorNumber(SP_ERROR_NULL_DEREF);
                    return false;
                }
                auto slice = rt_->NewSlice(array, index);
                if (!slice)
                    return false;
                vregs_[dest_reg] = rt_->heap().ToLocalAddr(slice.release());
                break;
            }
            case LL_IDXADDR_FLAT: {
                auto args = reader_.read<IdxAddrFlatArgs>();

                cell_t base = vregs_[args.base_reg];
                cell_t index = vregs_[args.index_reg];
                if (index < 0 || (uint32_t)index >= args.size) {
                    ReportOutOfBoundsError(index, args.size);
                    return false;
                }
                vregs_[args.dest_reg] = base + index * args.elt_size;
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
                uint8_t* dest = rt_->heap().ToPhysAddr<uint8_t*>(vregs_[dest_reg]);
                uint8_t* src = rt_->heap().ToPhysAddr<uint8_t*>(vregs_[src_reg]);
                memcpy(dest, src, bytes);
                break;
            }
            case LL_COPYARRAY_FLAT_A: {
                uint32_t count = reader_.read<uint32_t>();
                uint16_t src_reg = reader_.read<uint16_t>();
                uint16_t dest_reg = reader_.read<uint16_t>();

                cell_t* src = rt_->heap().ToPhysAddr<cell_t*>(vregs_[src_reg]);
                cell_t* dest = rt_->heap().ToPhysAddr<cell_t*>(vregs_[dest_reg]);

                for (uint32_t i = 0; i < count; i++) {
                    auto new_item = rt_->heap().ToPhysAddr<HeapItem*>(src[i]);
                    if (new_item && new_item->td->kind() == TypeKind::ArraySlice) {
                        rt_->ReportErrorNumber(SP_ERROR_SLICE_ESCAPE);
                        return false;
                    }
                    if (auto old_item = rt_->heap().ToPhysAddr<HeapItem*>(dest[i]))
                        old_item->Release();
                    dest[i] = src[i];
                    if (new_item)
                        new_item->AddRef();
                }
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
                auto args = reader_.read<SliceFlatArgs>();
                cell_t base = vregs_[args.base_reg];
                uint32_t index = vregs_[args.index_reg];
                auto slice = rt_->NewFlatSlice(base, args.td, index);
                if (!slice)
                    return false;
                vregs_[args.dest_reg] = rt_->heap().ToLocalAddr(slice.release());
                break;
            }

#define LOAD_ELEM_FLAT(c_type) \
                auto args = reader_.read<LoadElemFlatArgs>(); \
                uint32_t index = vregs_[args.index_reg]; \
                if (index >= args.array_size) { \
                    ReportOutOfBoundsError(index, args.array_size); \
                    return false; \
                } \
                auto elt = reinterpret_cast<c_type*>(&vregs_[args.base_reg]) + index

            case LL_LOAD_ELEM_FLAT_U8: {
                LOAD_ELEM_FLAT(uint8_t);
                vregs_[args.dest_reg] = *elt;
                break;
            }
            case LL_LOAD_ELEM_FLAT_I16: {
                LOAD_ELEM_FLAT(int16_t);
                vregs_[args.dest_reg] = *elt;
                break;
            }
            case LL_LOAD_ELEM_FLAT_I32:
            case LL_LOAD_ELEM_FLAT_F32: {
                LOAD_ELEM_FLAT(cell_t);
                vregs_[args.dest_reg] = *elt;
                break;
            }
            case LL_LOAD_ELEM_FLAT_I64: {
                LOAD_ELEM_FLAT(int64_t);
                *reinterpret_cast<int64_t*>(&vregs_[args.dest_reg]) = *elt;
                break;
            }
#undef LOAD_ELEM_FLAT

#define LOAD_ELEM_FLAT_I(c_type) \
                auto args = reader_.read<LoadElemFlatArgs>(); \
                uint32_t base = vregs_[args.base_reg]; \
                uint32_t index = vregs_[args.index_reg]; \
                if (index >= args.array_size) { \
                    ReportOutOfBoundsError(index, args.array_size); \
                    return false; \
                } \
                auto elt = rt_->heap().ToPhysAddr<c_type*>(base) + index

            case LL_LOAD_ELEM_FLAT_I_U8: {
                LOAD_ELEM_FLAT_I(uint8_t);
                vregs_[args.dest_reg] = *elt;
                break;
            }
            case LL_LOAD_ELEM_FLAT_I_I16: {
                LOAD_ELEM_FLAT_I(int16_t);
                vregs_[args.dest_reg] = *elt;
                break;
            }
            case LL_LOAD_ELEM_FLAT_I_I32:
            case LL_LOAD_ELEM_FLAT_I_F32: {
                LOAD_ELEM_FLAT_I(cell_t);
                vregs_[args.dest_reg] = *elt;
                break;
            }
            case LL_LOAD_ELEM_FLAT_I_I64: {
                LOAD_ELEM_FLAT_I(int64_t);
                *reinterpret_cast<int64_t*>(&vregs_[args.dest_reg]) = *elt;
                break;
            }
#undef LOAD_ELEM_FLAT_I

#define STOR_ELEM_FLAT(c_type) \
                auto args = reader_.read<StorElemFlatArgs>(); \
                uint32_t index = vregs_[args.index_reg]; \
                if (index >= args.array_size) { \
                    ReportOutOfBoundsError(index, args.array_size); \
                    return false; \
                } \
                auto elt = reinterpret_cast<c_type*>(&vregs_[args.base_reg]) + index

            case LL_STOR_ELEM_FLAT_I32:
            case LL_STOR_ELEM_FLAT_F32: {
                STOR_ELEM_FLAT(cell_t);
                *elt = vregs_[args.val_reg];
                break;
            }
            case LL_STOR_ELEM_FLAT_U8: {
                STOR_ELEM_FLAT(uint8_t);
                *elt = static_cast<uint8_t>(vregs_[args.val_reg]);
                break;
            }
            case LL_STOR_ELEM_FLAT_I16: {
                STOR_ELEM_FLAT(int16_t);
                *elt = static_cast<int16_t>(vregs_[args.val_reg]);
                break;
            }
            case LL_STOR_ELEM_FLAT_I64: {
                STOR_ELEM_FLAT(int64_t);
                *elt = *reinterpret_cast<int64_t*>(&vregs_[args.val_reg]);
                break;
            }
#undef STOR_ELEM_FLAT

#define STOR_ELEM_FLAT_I(c_type) \
                auto args = reader_.read<StorElemFlatArgs>(); \
                uint32_t base = vregs_[args.base_reg]; \
                uint32_t index = vregs_[args.index_reg]; \
                if (index >= args.array_size) { \
                    ReportOutOfBoundsError(index, args.array_size); \
                    return false; \
                } \
                auto elt = rt_->heap().ToPhysAddr<c_type*>(base) + index

            case LL_STOR_ELEM_FLAT_I_I32:
            case LL_STOR_ELEM_FLAT_I_F32: {
                STOR_ELEM_FLAT_I(cell_t);
                *elt = vregs_[args.val_reg];
                break;
            }
            case LL_STOR_ELEM_FLAT_I_U8: {
                STOR_ELEM_FLAT_I(uint8_t);
                *elt = static_cast<uint8_t>(vregs_[args.val_reg]);
                break;
            }
            case LL_STOR_ELEM_FLAT_I_I16: {
                STOR_ELEM_FLAT_I(int16_t);
                *elt = static_cast<int16_t>(vregs_[args.val_reg]);
                break;
            }
            case LL_STOR_ELEM_FLAT_I_I64: {
                STOR_ELEM_FLAT_I(int64_t);
                *elt = *reinterpret_cast<int64_t*>(&vregs_[args.val_reg]);
                break;
            }
#undef STOR_ELEM_FLAT_I

            BINARY_OP_I64(LL_SMUL_I64, *)
            BINARY_OP_I64(LL_ADD_I64, +)
            BINARY_OP_I64(LL_SUB_I64, -)
            BINARY_OP_I64(LL_SHL_I64, <<)
            BINARY_OP_I64(LL_SSHR_I64, >>)
            BINARY_OP_I64(LL_OR_I64, |)
            BINARY_OP_I64(LL_AND_I64, &)
            BINARY_OP_I64(LL_XOR_I64, ^)
            COMPARE_OP_I64(LL_EQ_I64, ==)
            COMPARE_OP_I64(LL_NEQ_I64, !=)
            COMPARE_OP_I64(LL_SLESS_I64, <)
            COMPARE_OP_I64(LL_SLEQ_I64, <=)
            COMPARE_OP_I64(LL_SGRTR_I64, >)
            COMPARE_OP_I64(LL_SGEQ_I64, >=)

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
            case LL_SHR_I64: {
                uint16_t a = reader_.read<uint16_t>();
                uint16_t b = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                uint64_t left = *reinterpret_cast<uint64_t*>(&vregs_[a]);
                uint64_t right = *reinterpret_cast<uint64_t*>(&vregs_[b]);
                *reinterpret_cast<uint64_t*>(&vregs_[dest]) = (left >> right);
                break;
            }
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
            case LL_RETN_A:
            case LL_RETV: {
                cell_t result = 0;
                if (op == LL_RETN || op == LL_RETN_A) {
                    uint16_t srcreg = reader_.read<uint16_t>();
                    result = vregs_[srcreg];
                }

                if (op == LL_RETN_A) {
                    if (auto item = rt_->heap().ToPhysAddr<HeapItem*>(result)) {
                        if (item->td->kind() == TypeKind::ArraySlice) {
                            rt_->ReportErrorNumber(SP_ERROR_SLICE_ESCAPE);
                            return false;
                        }
                        item->AddRef();
                    }
                }

                auto method = ivk_->callee()->method;
                method->llcode()->gcobj_regs().for_each([&](uintptr_t reg) {
                    cell_t val = vregs_[reg];
                    if (auto item = rt_->heap().ToPhysAddr<HeapItem*>(val))
                        item->Release();
                });

                if (ivk_ == &ivk) {
                    has_returned_ = true;
                    return_value_ = result;
                    break;
                }

                InterpFrame* frame = rt_->heap().ToPhysAddr<InterpFrame*>(frm_ - InterpFrame::AlignedSize());
                InterpFrame* prev = static_cast<InterpFrame*>(frame->prev());

                auto prev_method = prev->callee()->method;
                const uint8_t* caller_code = prev_method->llcode()->bytes();
                reader_ = BinaryReader(caller_code, caller_code + prev_method->llcode()->size());
                reader_.set_cursor(frame->saved_cip);
                ll_code = caller_code;

                uint32_t stack_amount = env_->sp() - (frm_ - InterpFrame::AlignedSize());
                if (!env_->dropStack(stack_amount))
                    return false;

                frm_ = frame->prev_frame;

                uint32_t num_caller_regs = prev_method->llcode()->num_regs();
                vregs_ = std::span<cell_t>(rt_->heap().ToPhysAddr<cell_t*>(frm_), num_caller_regs);

                if (frame->dest_reg != 0xFFFF)
                    vregs_[frame->dest_reg] = result;

                // Done reading the outgoing frame, we can pop it.
                frame->~InterpFrame();

                // Restore our pointer to the old frame.
                assert(prev == static_cast<InterpFrame*>(env_->top()));
                ivk_ = prev;
                ivk_->setCip(&insn_begin);
                break;
            }
            case LL_LOAD_FN: {
                uint32_t method_index = reader_.read<uint32_t>();
                uint16_t dest = reader_.read<uint16_t>();
                auto method = rt_->AcquireMethod(method_index);
                Handle<SpFunction> fn = method->GetFunction();
                if (!fn)
                    return false;
                vregs_[dest] = rt_->heap().ToLocalAddr(fn.release());
                break;
            }
            case LL_GETFUNCID: {
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                SpFunction* fn = rt_->heap().ToPhysAddr<SpFunction*>(vregs_[src]);
                if (!fn || !fn->method) {
                    rt_->ReportErrorNumber(SP_ERROR_NULL_DEREF);
                    return false;
                }
                vregs_[dest] = (fn->method->method_index() << 1) | 1;
                break;
            }
            case LL_GETFNOBJ: {
                uint16_t src = reader_.read<uint16_t>();
                const TypeDesc* td = reader_.read<const TypeDesc*>();
                uint16_t dest = reader_.read<uint16_t>();

                Handle<SpFunction> fun = rt_->CastFunctionId(vregs_[src], td);
                if (!fun)
                    return false;

                vregs_[dest] = rt_->heap().ToLocalAddr(fun.release());
                break;
            }
            case LL_LOAD_STR: {
                uint32_t index = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                cell_t str = rt_->GetStringAddr(index);
                vregs_[dest] = str;
                heap_.ToPhysAddr<HeapItem*>(str)->AddRef();
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
            case LL_LOAD_FLD_A: {
                uint32_t offset = reader_.read<uint32_t>();
                uint16_t base = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                uint8_t* base_ptr = rt_->heap().ToPhysAddr<uint8_t*>(vregs_[base]);
                vregs_[dest] = *reinterpret_cast<cell_t*>(base_ptr + offset);
                if (vregs_[dest])
                    heap_.ToPhysAddr<HeapItem*>(vregs_[dest])->AddRef();
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
            case LL_STOR_FLD_A: {
                uint32_t offset = reader_.read<uint32_t>();
                uint16_t basereg = reader_.read<uint16_t>();
                uint16_t valreg = reader_.read<uint16_t>();
                cell_t obj_addr = vregs_[basereg];
                uint8_t* base_ptr = rt_->heap().ToPhysAddr<uint8_t*>(obj_addr);
                cell_t* ptr = reinterpret_cast<cell_t*>(base_ptr + offset);
                auto new_item = heap_.ToPhysAddr<HeapItem*>(vregs_[valreg]);
                if (new_item && new_item->td->kind() == TypeKind::ArraySlice) {
                    rt_->ReportErrorNumber(SP_ERROR_SLICE_ESCAPE);
                    return false;
                }
                if (auto old_item = heap_.ToPhysAddr<HeapItem*>(*ptr))
                    old_item->Release();
                if (new_item)
                    new_item->AddRef();
                *ptr = vregs_[valreg];
                break;
            }
            case LL_NTVCALL:
            case LL_NTVCALL_VA: {
                uint32_t native_index = reader_.read<uint32_t>();
                uint8_t nargs = reader_.read<uint8_t>();
                uint16_t spread_reg = LL_INVALID_REG;
                if (op == LL_NTVCALL_VA)
                    spread_reg = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();


                uint32_t vararg_count = 0;
                cell_t* varargs = nullptr;
                if (spread_reg != LL_INVALID_REG) {
                    varargs = rt_->heap().ToPhysAddr<cell_t*>(vregs_[spread_reg]);
                    vararg_count = varargs[0];
                }

                ke::SaveRestore<uint32_t> save_sp(env_->sp());

                uint32_t total_args = nargs + vararg_count;
                uint32_t params_size = (total_args + 1) * sizeof(cell_t);
                uint32_t stack_base = env_->sp();
                if (!env_->addStack(params_size))
                    return false;

                cell_t* params = rt_->heap().ToPhysAddr<cell_t*>(stack_base);

                params[0] = total_args;
                for (uint8_t i = 0; i < nargs; i++) {
                    uint16_t arg_reg = reader_.read<uint16_t>();
                    params[i + 1] = vregs_[arg_reg];
                }
                for (cell_t i = 0; i < vararg_count; i++)
                    params[nargs + i + 1] = varargs[i + 1];

                NativeEntry* native = rt_->NativeAt(native_index);
                ivk_->enterNativeCall(native_index);

                if (native->status == SP_NATIVE_BOUND) {
                    cell_t result;
                    if (native->legacy_fn)
                        result = native->legacy_fn(rt_, params);
                    else
                        result = native->callback->Invoke(rt_, params);

                    if (dest != 0xFFFF)
                        vregs_[dest] = result;
                } else {
                    rt_->ReportErrorNumber(SP_ERROR_INVALID_NATIVE);
                }

                ivk_->leaveNativeCall();

                if (env_->hasPendingException())
                    return false;

                break;
            }
            case LL_CALLI:
            case LL_CALL: {
                RefPtr<MethodInfo> target;
                SpFunction* indirect_callee = nullptr;
                if (op == LL_CALL) {
                    auto* method = reader_.read<const smx_rtti_method*>();
                    target = rt_->AcquireMethod(smx_->GetIndexOfMethod(method));
                } else {
                    uint16_t fn_reg = reader_.read<uint16_t>();
                    auto* sp_fn = rt_->heap().ToPhysAddr<SpFunction*>(vregs_[fn_reg]);
                    if (!sp_fn || !sp_fn->method) {
                        rt_->ReportErrorNumber(SP_ERROR_NULL_DEREF);
                        return false;
                    }
                    indirect_callee = sp_fn;
                    target = sp_fn->method;
                }
                uint8_t nargs = reader_.read<uint8_t>();
                uint16_t dest = reader_.read<uint16_t>();

                if (!target->Validate())
                    return false;

                if (!target->llcode()) {
                    ke::RefPtr<ControlFlowGraph> graph = target->BuildGraph();
                    if (!graph)
                        return false;
                    std::unique_ptr<LLCode> code = LowerMethod(graph, target.get());
                    target->set_llcode(std::move(code));
                }

                uint32_t target_nargs = target->arg_types().size();
                uint32_t callee_regs = target->llcode()->num_regs() - target_nargs;
                uint32_t stack_amount = InterpFrame::AlignedSize() +
                                        target_nargs * sizeof(cell_t) +
                                        callee_regs * sizeof(cell_t);

                uint32_t frame_base = env_->sp();
                if (!env_->addStack(stack_amount))
                    return false;

                uint32_t new_frm = frame_base + InterpFrame::AlignedSize();
                cell_t* new_vregs = rt_->heap().ToPhysAddr<cell_t*>(new_frm);
                for (uint8_t i = 0; i < nargs; i++) {
                    uint16_t arg_reg = reader_.read<uint16_t>();
                    new_vregs[i] = vregs_[arg_reg];
                }

                SpFunction* callee = (op == LL_CALLI) ? indirect_callee : target->GetFunction().get();
                InterpFrame* frame = new (rt_->heap().ToPhysAddr<uint8_t*>(frame_base))
                    InterpFrame(callee, rt_, &insn_begin,
                                reader_.cursor(), dest, frm_);

                if (callee_regs > 0)
                    memset(&new_vregs[nargs], 0, callee_regs * sizeof(cell_t));

                ivk_->setCip(&frame->saved_cip);

                frm_ = new_frm;
                ivk_ = frame;
                vregs_ = std::span<cell_t>(new_vregs, target->llcode()->num_regs());

                auto method = frame->callee()->method;
                const uint8_t* callee_code = method->llcode()->bytes();
                reader_ = BinaryReader(callee_code, callee_code + method->llcode()->size());
                ll_code = callee_code;
                continue;
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
            BINARY_OP_I32(LL_SHL_I32, <<)
            BINARY_OP_I32(LL_SSHR_I32, >>)
            case LL_SHR_I32: {
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
            BINARY_OP_I32(LL_AND_I32, &)
            BINARY_OP_I32(LL_OR_I32, |)
            BINARY_OP_I32(LL_XOR_I32, ^)
            BINARY_OP_I32(LL_EQ_I32, ==)
            BINARY_OP_I32(LL_NEQ_I32, !=)
            BINARY_OP_I32(LL_SLESS_I32, <)
            BINARY_OP_I32(LL_SLEQ_I32, <=)
            BINARY_OP_I32(LL_SGRTR_I32, >)
            BINARY_OP_I32(LL_SGEQ_I32, >=)
            case LL_NOT_I32: {
                uint16_t a = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = vregs_[a] ? 0 : 1;
                break;
            }
            case LL_NEG_I32: {
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = -vregs_[src];
                break;
            }
            case LL_INVERT_I32: {
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
            case LL_CVT_I16: {
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = (cell_t)(int16_t)vregs_[src];
                break;
            }
            case LL_TRUNCATE_I64: {
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = (cell_t)*reinterpret_cast<int64_t*>(&vregs_[src]);
                break;
            }
            case LL_COPYARRAY: {
                uint32_t bytes = reader_.read<uint32_t>();
                uint16_t srcreg = reader_.read<uint16_t>();
                uint16_t destreg = reader_.read<uint16_t>();
                SpArray* src = heap_.ToPhysAddr<SpArray*>(vregs_[srcreg]);
                SpArray* dest = heap_.ToPhysAddr<SpArray*>(vregs_[destreg]);
                if (!src) {
                    rt_->ReportErrorNumber(SP_ERROR_NULL_DEREF);
                    return false;
                }
                assert(dest->td->kind() == TypeKind::FixedArray);

                auto src_data = heap_.ToPhysAddr<uint8_t*>(src->data);
                auto dest_data = heap_.ToPhysAddr<uint8_t*>(dest->data);

                if (src_data == dest_data)
                    break;

                memcpy(dest_data, src_data, bytes);
                break;
            }
            case LL_COPYARRAY_A: {
                uint16_t srcreg = reader_.read<uint16_t>();
                uint16_t destreg = reader_.read<uint16_t>();
                SpArray* src = heap_.ToPhysAddr<SpArray*>(vregs_[srcreg]);
                SpArray* dest = heap_.ToPhysAddr<SpArray*>(vregs_[destreg]);
                if (!src) {
                    rt_->ReportErrorNumber(SP_ERROR_NULL_DEREF);
                    return false;
                }
                assert(dest->td->kind() == TypeKind::FixedArray);

                auto src_data = heap_.ToPhysAddr<cell_t*>(src->data);
                auto dest_data = heap_.ToPhysAddr<cell_t*>(dest->data);

                if (src_data == dest_data)
                    break;

                uint32_t count = dest->length;
                for (uint32_t i = 0; i < count; i++) {
                    auto new_item = heap_.ToPhysAddr<HeapItem*>(src_data[i]);
                    if (new_item && new_item->td->kind() == TypeKind::ArraySlice) {
                        rt_->ReportErrorNumber(SP_ERROR_SLICE_ESCAPE);
                        return false;
                    }
                    if (auto old_item = heap_.ToPhysAddr<HeapItem*>(dest_data[i]))
                        old_item->Release();
                    dest_data[i] = src_data[i];
                    if (new_item)
                        new_item->AddRef();
                }
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
                cell_t val = vregs_[size_reg];
                if (val < 0) {
                    rt_->ReportErrorNumber(SP_ERROR_ARRAY_BOUNDS);
                    return false;
                }
                auto array = rt_->NewArray(td, val);
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
            case LL_NEWOBJ: {
                auto td = reader_.read<const TypeDesc*>();
                uint16_t dest = reader_.read<uint16_t>();
                auto obj = rt_->NewObject(td);
                if (!obj)
                    return false;
                vregs_[dest] = rt_->heap().ToLocalAddr(obj.release());
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
                uint16_t reg = reader_.read<uint16_t>();
                auto array = rt_->heap().ToPhysAddr<SpArray*>(vregs_[reg]);
                rt_->FillArray(array, data_offset);
                break;
            }
            case LL_SLICE_ES: {
                uint32_t cell_count = reader_.read<uint32_t>();
                uint16_t src = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();

                auto slice = rt_->NewSliceEs(vregs_[src], cell_count);
                if (!slice)
                    return false;
                vregs_[dest] = rt_->heap().ToLocalAddr(slice.release());
                break;
            }
            case LL_ADDR_S: {
                uint16_t reg = reader_.read<uint16_t>();
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = rt_->heap().ToLocalAddr(&vregs_[reg]);
                break;
            }
            case LL_RELEASE: {
                uint16_t reg = reader_.read<uint16_t>();
                if (auto item = rt_->heap().ToPhysAddr<HeapItem*>(vregs_[reg])) {
                    item->Release();
                    vregs_[reg] = 0;
                }
                break;
            }
            case LL_ADDREF: {
                uint16_t reg = reader_.read<uint16_t>();
                if (auto item = rt_->heap().ToPhysAddr<HeapItem*>(vregs_[reg]))
                    item->AddRef();
                break;
            }
            case LL_STOR_S_A: {
                uint16_t src_reg = reader_.read<uint16_t>();
                uint16_t dest_reg = reader_.read<uint16_t>();
                if (auto new_item = heap_.ToPhysAddr<HeapItem*>(vregs_[src_reg]))
                    new_item->AddRef();
                if (auto old_item = heap_.ToPhysAddr<HeapItem*>(vregs_[dest_reg]))
                    old_item->Release();
                vregs_[dest_reg] = vregs_[src_reg];
                break;
            }
            case LL_CALLEE: {
                uint16_t dest = reader_.read<uint16_t>();
                vregs_[dest] = rt_->heap().ToLocalAddr(ivk_->callee());
                break;
            }
            case LL_NEWCLOSURE: {
                auto closure_td = reader_.read<const TypeDesc*>();
                uint32_t method_id = reader_.read<uint32_t>();
                uint16_t dest = reader_.read<uint16_t>();
                auto method = rt_->AcquireMethod(method_id);
                auto fn = rt_->NewClosure(closure_td, method.get());
                if (!fn)
                    return false;
                vregs_[dest] = rt_->heap().ToLocalAddr(fn.release());
                break;
            }
            case LL_ADDR_UPVAR: {
                auto args = reader_.read<UpvarArgs>();
                SpFunction* fn = heap_.ToPhysAddr<SpFunction*>(vregs_[args.closure_reg]);
                cell_t* upvar_addr = reinterpret_cast<cell_t*>(fn->upvars() + args.slot);
                vregs_[args.reg] = rt_->heap().ToLocalAddr(upvar_addr);
                break;
            }
            case LL_LOAD_UPVAR_X32: {
                auto args = reader_.read<UpvarArgs>();
                SpFunction* fn = heap_.ToPhysAddr<SpFunction*>(vregs_[args.closure_reg]);
                vregs_[args.reg] = *reinterpret_cast<cell_t*>(fn->upvars() + args.slot);
                break;
            }
            case LL_LOAD_UPVAR_X64: {
                auto args = reader_.read<UpvarArgs>();
                SpFunction* fn = heap_.ToPhysAddr<SpFunction*>(vregs_[args.closure_reg]);
                *reinterpret_cast<int64_t*>(&vregs_[args.reg]) =
                    *reinterpret_cast<int64_t*>(fn->upvars() + args.slot);
                break;
            }
            case LL_LOAD_UPVAR_A: {
                auto args = reader_.read<UpvarArgs>();
                SpFunction* fn = heap_.ToPhysAddr<SpFunction*>(vregs_[args.closure_reg]);
                cell_t val = *reinterpret_cast<cell_t*>(fn->upvars() + args.slot);
                if (val)
                    heap_.ToPhysAddr<HeapItem*>(val)->AddRef();
                vregs_[args.reg] = val;
                break;
            }
            case LL_STOR_UPVAR_X32: {
                auto args = reader_.read<UpvarArgs>();
                SpFunction* fn = heap_.ToPhysAddr<SpFunction*>(vregs_[args.closure_reg]);
                *reinterpret_cast<cell_t*>(fn->upvars() + args.slot) = vregs_[args.reg];
                break;
            }
            case LL_STOR_UPVAR_X64: {
                auto args = reader_.read<UpvarArgs>();
                SpFunction* fn = heap_.ToPhysAddr<SpFunction*>(vregs_[args.closure_reg]);
                *reinterpret_cast<int64_t*>(fn->upvars() + args.slot) =
                    *reinterpret_cast<int64_t*>(&vregs_[args.reg]);
                break;
            }
            case LL_STOR_UPVAR_A: {
                auto args = reader_.read<UpvarArgs>();
                SpFunction* fn = heap_.ToPhysAddr<SpFunction*>(vregs_[args.closure_reg]);
                cell_t* slot = reinterpret_cast<cell_t*>(fn->upvars() + args.slot);
                cell_t val = vregs_[args.reg];
                if (auto new_item = heap_.ToPhysAddr<HeapItem*>(val)) {
                    if (new_item->td->kind() == TypeKind::ArraySlice) {
                        rt_->ReportErrorNumber(SP_ERROR_SLICE_ESCAPE);
                        return false;
                    }
                    new_item->AddRef();
                }
                if (auto old_item = heap_.ToPhysAddr<HeapItem*>(*slot))
                    old_item->Release();
                *slot = val;
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

void Interpreter::UnwindStack(InterpInvokeFrame* root_ivk, MethodInfo* root_method,
                               std::span<cell_t> root_vregs)
{
    while (env_->top() != root_ivk) {
        InterpFrame* frame = static_cast<InterpFrame*>(env_->top());
        SpFunction* callee = frame->callee();
        cell_t* unwound_vregs = frame->vregs();

        callee->method->llcode()->gcobj_regs().for_each([&](uintptr_t reg) {
            cell_t val = unwound_vregs[reg];
            if (auto item = rt_->heap().ToPhysAddr<HeapItem*>(val))
                item->Release();
        });

        frame->~InterpFrame();
    }
    if (!has_returned_ && !root_vregs.empty()) {
        root_method->llcode()->gcobj_regs().for_each([&](uintptr_t reg) {
            cell_t val = root_vregs[reg];
            if (auto item = rt_->heap().ToPhysAddr<HeapItem*>(val))
                item->Release();
        });
    }
}

InterpFrame::InterpFrame(SpFunction* sp_fn, BaseRuntime* cx, const uint8_t** initial_cip,
                         const uint8_t* saved_cip, uint32_t dest_reg, uint32_t prev_frame)
 : InterpInvokeFrame(cx, sp_fn, initial_cip),
   saved_cip(saved_cip),
   dest_reg(dest_reg),
   prev_frame(prev_frame)
{
    callee_->AddRef();
}

InterpFrame::~InterpFrame() {
    callee_->Release();
}

BaseMethodInfo* InterpFrame::method() const {
    return callee_->method;
}

} // namespace sp::v2
