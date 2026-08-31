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
#include "v2/interp/interp-code.h"
#include "v2/interp/ll-op.h"
#include "v2/interp/lowering.h"
#include "v2/method-info.h"
#include "v2/objects.h"
#include "v2/pcode-reader.h"
#include "v2/runtime-helpers.h"
#include "v2/runtime.h"
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
    if (!interpreter.run())
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
   frm_(cx->sp()),
   phys_frm_(cx->heap().ToPhysAddr<cell_t*>(frm_))
{}

bool Interpreter::run() {
    const uint8_t* insn_begin = reader_.cursor();
    const uint8_t* ll_code = method_->interp()->bytes();

    InterpInvokeFrame ivk(rt_, method_, &insn_begin);
    ke::SaveAndSet<InterpInvokeFrame*> enterIvk(&ivk_, &ivk);
    ke::SaveRestore<uint32_t> saveSp(rt_->sp());

    const smx_rtti_method* rtti = smx_->GetMethod(method_->method_index());
    bool is_global_ctor = (rtti->flags & kRttiMethod_GlobalCtor) != 0;

    std::optional<ke::SaveRestore<uint32_t>> saveHpScope;
    std::optional<HeapSave> saveHp;
    if (!is_global_ctor) {
        saveHpScope.emplace(rt_->hp_scope());
        saveHp.emplace(rt_->heap());
    }

    cell_t locals_size = CalcLocalsSize();
    assert(locals_size >= 0);

    if (locals_size && !rt_->addStack(-locals_size))
        return false;

    uint32_t eval_depth = method_->max_eval_stack_depth();
    uint32_t stack_bytes = method_->max_eval_stack_bytes();
    assert(stack_bytes % sizeof(cell_t) == 0);

    if (stack_bytes + eval_depth > 0) {
        // Round up depth to keep sp_ aligned.
        eval_depth = ke::Align(eval_depth, sizeof(cell_t));
        if (!rt_->addStack(-(cell_t)(stack_bytes + eval_depth)))
            return false;

        uint8_t* base = rt_->heap().ToPhysAddr<uint8_t*>(rt_->sp());

        // We reserve two chunks of data off the stack.
        //    "eval_stack", which holds the operand stack.
        //    "stack_types", which holds the types of entries on the operand stack.
        // This distinction is needed because some operands are two cells instead
        // of one (eg for 64-bit values).
        //
        // The operand stack is placed at lower addresses, so we can use a sliding
        // window mechanism into the new function call.
        uint8_t* sp_bottom = base;
        uint8_t* sp_top = base + stack_bytes;

        eval_stack_top_ = reinterpret_cast<cell_t*>(sp_top);
        eval_stack_limit_ = reinterpret_cast<cell_t*>(sp_bottom);
        eval_stack_ptr_ = eval_stack_top_;

        stack_types_top_ = sp_top + eval_depth;
        stack_types_limit_ = sp_top;
        stack_types_ptr_ = stack_types_top_;
    }

    if (!InitLocals())
        return false;

    while (!has_returned_ && reader_.more()) {
        insn_begin = reader_.cursor();

        if (Environment::get()->IsDebugBreakEnabled()) {
            uint32_t ll_offset = (uint32_t)(insn_begin - ll_code);
            uint32_t high_offset = method_->interp()->LookupHighOffset(ll_offset);
            if (smx_->IsLineBoundary(high_offset)) {
                InvokeDebugger(rt_, nullptr);
                if (env_->hasPendingException())
                    return false;
            }
        }

        if (env_->spew_interp_ops()) {
            LLOp op = (LLOp)*reinterpret_cast<const uint16_t*>(insn_begin);
            fprintf(stdout, "  [%05u] %s\n", (uint32_t)(insn_begin - ll_code), GetLLOpName(op));
        }

        LLOp op = (LLOp)reader_.read<uint16_t>();

        switch (op) {
            case LL_NOP:
                break;
            case LL_LOAD_GLB: {
                uint16_t index = reader_.read<uint16_t>();
                cell_t addr = rt_->GetGlobalAddr(index);
                const TypeDesc* td = rt_->GetTypeOfGlobal(index);
                if (td->IsInt64()) {
                    int64_t* ptr = rt_->acquireInt64Addr(addr);
                    if (!ptr)
                        return false;
                    pushInt64(*ptr);
                } else {
                    cell_t val;
                    if (!rt_->getCellValue(addr, &val))
                        return false;
                    pushCell(val);
                }
                break;
            }
            case LL_LOAD_S: {
                cell_t offset = reader_.readInt16();
                const TypeDesc* td = method_->GetTypeOfLocal(offset);
                if (td->IsInt64())
                    pushInt64(getLocalInt64(offset));
                else
                    pushCell(getLocalCell(offset));
                break;
            }
            case LL_LOAD_I_I32:
            case LL_LOAD_I_F32: {
                cell_t addr = popCell();
                cell_t val;
                if (!rt_->getCellValue(addr, &val))
                    return false;
                pushCell(val);
                break;
            }
            case LL_LOAD_I_I64: {
                cell_t addr = popCell();
                int64_t* ptr = rt_->acquireInt64Addr(addr);
                if (!ptr)
                    return false;
                pushInt64(*ptr);
                break;
            }
            case LL_LOAD_I_U8: {
                cell_t addr = popCell();
                cell_t val;
                if (!rt_->getCellValue(addr, &val))
                    return false;
                val &= 0xff;
                pushCell(val);
                break;
            }
            case LL_LOAD_ELEM_A:
            case LL_LOAD_ELEM_I32:
            case LL_LOAD_ELEM_F32: {
                uint32_t index = popCell();
                uint32_t base = popCell();
                auto array = rt_->heap().ToPhysAddr<SpArray*>(base);
                if (index >= array->length) {
                    ReportOutOfBoundsError(index, array->length);
                    return false;
                }
                void* elt = rt_->GetArrayElem(array, index);
                pushCell(*reinterpret_cast<cell_t*>(elt));
                break;
            }
            case LL_LOAD_ELEM_I64: {
                uint32_t index = popCell();
                uint32_t base = popCell();
                auto array = rt_->heap().ToPhysAddr<SpArray*>(base);
                if (index >= array->length) {
                    ReportOutOfBoundsError(index, array->length);
                    return false;
                }
                void* elt = rt_->GetArrayElem(array, index);
                pushInt64(*reinterpret_cast<int64_t*>(elt));
                break;
            }
            case LL_LOAD_ELEM_U8: {
                uint32_t index = popCell();
                uint32_t base = popCell();
                auto array = rt_->heap().ToPhysAddr<SpArray*>(base);
                if (index >= array->length) {
                    ReportOutOfBoundsError(index, array->length);
                    return false;
                }
                void* elt = rt_->GetArrayElem(array, index);
                pushCell(*reinterpret_cast<uint8_t*>(elt));
                break;
            }
            case LL_ADDR_GLB: {
                uint16_t index = reader_.read<uint16_t>();
                pushCell(rt_->GetGlobalAddr(index));
                break;
            }
            case LL_STOR_GLB: {
                uint16_t index = reader_.read<uint16_t>();
                cell_t addr = rt_->GetGlobalAddr(index);
                const TypeDesc* td = rt_->GetTypeOfGlobal(index);
                if (td->IsInt64()) {
                    int64_t val = popInt64();
                    int64_t* ptr = rt_->acquireInt64Addr(addr);
                    if (!ptr)
                        return false;
                    *ptr = val;
                } else {
                    cell_t val = popCell();
                    if (!rt_->setCellValue(addr, val))
                        return false;
                }
                break;
            }
            case LL_STOR_S: {
                cell_t offset = reader_.readInt16();
                const TypeDesc* td = method_->GetTypeOfLocal(offset);
                if (td->IsInt64()) {
                    int64_t val = popInt64();
                    getLocalInt64(offset) = val;
                } else {
                    cell_t val = popCell();
                    setLocalCell(offset, val);
                }
                break;
            }
            case LL_STOR_S_C: {
                cell_t slot = reader_.readInt16();
                cell_t value = reader_.readCell();
                setLocalCell(slot, value);
                break;
            }
            case LL_STOR_I_I32:
            case LL_STOR_I_F32: {
                cell_t val = popCell();
                cell_t addr = popCell();
                if (!rt_->setCellValue(addr, val))
                    return false;
                break;
            }
            case LL_STOR_I_I64: {
                int64_t val = popInt64();
                cell_t addr = popCell();
                int64_t* ptr = rt_->acquireInt64Addr(addr);
                if (!ptr)
                    return false;
                *ptr = val;
                break;
            }
            case LL_STOR_I_U8: {
                cell_t val = popCell();
                cell_t addr_val = popCell();
                uint8_t* addr = rt_->heap().ToPhysAddr<uint8_t*>(addr_val);
                if (!addr)
                    return false;
                *addr = uint8_t(val);
                break;
            }
            case LL_STOR_ELEM_I32:
            case LL_STOR_ELEM_F32: {
                cell_t val = popCell();
                uint32_t index = popCell();
                uint32_t base = popCell();
                auto array = rt_->heap().ToPhysAddr<SpArray*>(base);
                if (index >= array->length) {
                    ReportOutOfBoundsError(index, array->length);
                    return false;
                }
                void* elt = rt_->GetArrayElem(array, index);
                *reinterpret_cast<cell_t*>(elt) = val;
                break;
            }
            case LL_STOR_ELEM_I64: {
                int64_t val = popInt64();
                uint32_t index = popCell();
                uint32_t base = popCell();
                auto array = rt_->heap().ToPhysAddr<SpArray*>(base);
                if (index >= array->length) {
                    ReportOutOfBoundsError(index, array->length);
                    return false;
                }
                void* elt = rt_->GetArrayElem(array, index);
                *reinterpret_cast<int64_t*>(elt) = val;
                break;
            }
            case LL_STOR_ELEM_U8: {
                cell_t val = popCell();
                uint32_t index = popCell();
                uint32_t base = popCell();
                auto array = rt_->heap().ToPhysAddr<SpArray*>(base);
                if (index >= array->length) {
                    ReportOutOfBoundsError(index, array->length);
                    return false;
                }
                void* elt = rt_->GetArrayElem(array, index);
                *reinterpret_cast<uint8_t*>(elt) = uint8_t(val);
                break;
            }
            case LL_IDXADDR: {
                uint32_t index = popCell();
                uint32_t base = popCell();
                auto array = rt_->heap().ToPhysAddr<SpArray*>(base);
                if (index >= array->length) {
                    ReportOutOfBoundsError(index, array->length);
                    return false;
                }

                void* elt_addr = rt_->GetArrayElem(array, index);
                pushCell(rt_->heap().ToLocalAddr(elt_addr));
                break;
            }
            case LL_SLICE: {
                uint32_t index = popCell();
                uint32_t base = popCell();
                auto array = rt_->heap().ToPhysAddr<SpArray*>(base);
                if (index >= array->length) {
                    ReportOutOfBoundsError(index, array->length);
                    return false;
                }
                auto slice = rt_->NewSlice(array, index);
                if (!slice)
                    return false;
                pushCell(rt_->heap().ToLocalAddr(slice));
                break;
            }
            case LL_IDXADDR_FLAT: {
                uint32_t size = reader_.read<uint32_t>();
                uint32_t elt_size = reader_.read<uint32_t>();
                cell_t index = popCell();
                cell_t base = popCell();
                if (index < 0 || (uint32_t)index >= size) {
                    ReportOutOfBoundsError(index, size);
                    return false;
                }
                pushCell(base + index * elt_size);
                break;
            }
            case LL_ARRAY_TO_FLAT: {
                cell_t addr = popCell();
                auto array = rt_->heap().ToPhysAddr<SpArray*>(addr);
                pushCell(array->data);
                break;
            }
            case LL_COPYARRAY_FLAT: {
                uint32_t bytes = reader_.read<uint32_t>();
                cell_t src_addr = popCell();
                cell_t dest_addr = popCell();
                uint8_t* dest = rt_->heap().ToPhysAddr<uint8_t*>(dest_addr);
                uint8_t* src = rt_->heap().ToPhysAddr<uint8_t*>(src_addr);
                memcpy(dest, src, bytes);
                break;
            }
            case LL_FILLARRAY_FLAT: {
                uint32_t data_offs = reader_.read<uint32_t>();
                const TypeDesc* td = reader_.read<const TypeDesc*>();
                cell_t local_addr = popCell();
                rt_->FillFlatArray(local_addr, td, data_offs);
                break;
            }
            case LL_SLICE_FLAT: {
                const TypeDesc* td = reader_.read<const TypeDesc*>();
                uint32_t index = popCell();
                cell_t base = popCell();
                auto slice = rt_->NewFlatSlice(base, td, index);
                if (!slice)
                    return false;
                pushCell(rt_->heap().ToLocalAddr(slice));
                break;
            }

#define DO_STOR_ELEM(pop_type, val_type) { \
                uint32_t size = reader_.read<uint32_t>(); \
                uint32_t elt_size = reader_.read<uint32_t>(); \
                val_type val = pop##pop_type(); \
                cell_t index = popCell(); \
                cell_t base = popCell(); \
                if (index < 0 || (uint32_t)index >= size) { \
                    ReportOutOfBoundsError(index, size); \
                    return false; \
                } \
                val_type* addr = rt_->heap().ToPhysAddr<val_type*>(base + index * elt_size); \
                *addr = val; \
            }

            case LL_STOR_ELEM_FLAT_I32:
            case LL_STOR_ELEM_FLAT_F32:
                DO_STOR_ELEM(Cell, cell_t);
                break;
            case LL_STOR_ELEM_FLAT_I64:
                DO_STOR_ELEM(Int64, int64_t);
                break;
            case LL_STOR_ELEM_FLAT_U8:
                DO_STOR_ELEM(Cell, uint8_t);
                break;

#undef DO_STOR_ELEM

            case LL_POP: {
                popStack();
                break;
            }
            case LL_DUP: {
                StackValue v = popValue();
                pushValue(v);
                pushValue(v);
                break;
            }
            case LL_SWAP: {
                StackValue b = popValue();
                StackValue a = popValue();
                pushValue(b);
                pushValue(a);
                break;
            }
            case LL_PUSH_C: {
                cell_t val = reader_.readCell();
                pushCell(val);
                break;
            }
            case LL_PUSH_C_I8: {
                int8_t val = reader_.read<int8_t>();
                pushCell(val);
                break;
            }
            case LL_PUSH_C_I64: {
                int64_t val = reader_.read<int64_t>();
                pushInt64(val);
                break;
            }
            case LL_CVT_I64: {
                cell_t val = popCell();
                pushInt64((int64_t)val);
                break;
            }
            case LL_TRUNCATE_I64: {
                int64_t val = popInt64();
                pushCell((cell_t)val);
                break;
            }
            case LL_TEST_I64: {
                int64_t val = popInt64();
                pushCell(!!val);
                break;
            }
            case LL_INVERT_I64: {
                int64_t val = popInt64();
                pushInt64(~val);
                break;
            }
            case LL_NEG_I64: {
                int64_t val = popInt64();
                pushInt64(-val);
                break;
            }
            case LL_SMUL_I64: {
                int64_t right = popInt64();
                int64_t left = popInt64();
                pushInt64(left * right);
                break;
            }
            case LL_SDIV_I64: {
                int64_t right = popInt64();
                int64_t left = popInt64();
                int64_t result;
                int err = Int64Div(&right, &left, &result);
                if (err != SP_ERROR_NONE) {
                    rt_->ReportErrorNumber(err);
                    return false;
                }
                pushInt64(result);
                break;
            }
            case LL_SMOD_I64: {
                int64_t right = popInt64();
                int64_t left = popInt64();
                int64_t result;
                int err = Int64Mod(&right, &left, &result);
                if (err != SP_ERROR_NONE) {
                    rt_->ReportErrorNumber(err);
                    return false;
                }
                pushInt64(result);
                break;
            }
            case LL_ADD_I64: {
                int64_t right = popInt64();
                int64_t left = popInt64();
                pushInt64(left + right);
                break;
            }
            case LL_SUB_I64: {
                int64_t right = popInt64();
                int64_t left = popInt64();
                pushInt64(left - right);
                break;
            }
            case LL_SHL_I64: {
                int64_t right = popInt64();
                int64_t left = popInt64();
                pushInt64(left << right);
                break;
            }
            case LL_SSHR_I64: {
                int64_t right = popInt64();
                int64_t left = popInt64();
                pushInt64(left >> right);
                break;
            }
            case LL_SHR_I64: {
                int64_t right = popInt64();
                int64_t left = popInt64();
                pushInt64(uint64_t(left) >> uint64_t(right));
                break;
            }
            case LL_OR_I64: {
                int64_t right = popInt64();
                int64_t left = popInt64();
                pushInt64(left | right);
                break;
            }
            case LL_AND_I64: {
                int64_t right = popInt64();
                int64_t left = popInt64();
                pushInt64(left & right);
                break;
            }
            case LL_XOR_I64: {
                int64_t right = popInt64();
                int64_t left = popInt64();
                pushInt64(left ^ right);
                break;
            }
            case LL_RETN: {
                has_returned_ = true;
                return_value_ = popCell();
                break;
            }
            case LL_RETV: {
                has_returned_ = true;
                break;
            }
            case LL_LOAD_FN: {
                uint32_t method_index = (uint32_t)reader_.readCell();
                funcid_t id = (method_index << 1) | 1;
                pushCell(id);
                break;
            }
            case LL_LOAD_STR: {
                uint16_t index = reader_.read<uint16_t>();
                pushCell(rt_->GetStringAddr(index));
                break;
            }
            case LL_LOAD_FLD_X32: {
                uint32_t offset = reader_.read<uint32_t>();
                cell_t obj_addr = popCell();
                uint8_t* base_ptr = rt_->heap().ToPhysAddr<uint8_t*>(obj_addr);
                pushCell(*reinterpret_cast<cell_t*>(base_ptr + offset));
                break;
            }
            case LL_LOAD_FLD_X64: {
                uint32_t offset = reader_.read<uint32_t>();
                cell_t obj_addr = popCell();
                uint8_t* base_ptr = rt_->heap().ToPhysAddr<uint8_t*>(obj_addr);
                pushInt64(*reinterpret_cast<int64_t*>(base_ptr + offset));
                break;
            }
            case LL_ADDR_FLD: {
                uint32_t offset = reader_.read<uint32_t>();
                cell_t obj_addr = popCell();
                pushCell(obj_addr + offset);
                break;
            }
            case LL_STOR_FLD_X32: {
                uint32_t offset = reader_.read<uint32_t>();
                cell_t val = popCell();
                cell_t obj_addr = popCell();
                uint8_t* base_ptr = rt_->heap().ToPhysAddr<uint8_t*>(obj_addr);
                *reinterpret_cast<cell_t*>(base_ptr + offset) = val;
                break;
            }
            case LL_STOR_FLD_X64: {
                uint32_t offset = reader_.read<uint32_t>();
                int64_t val = popInt64();
                cell_t obj_addr = popCell();
                uint8_t* base_ptr = rt_->heap().ToPhysAddr<uint8_t*>(obj_addr);
                *reinterpret_cast<int64_t*>(base_ptr + offset) = val;
                break;
            }
            case LL_CALL:
            case LL_CALLN: {
                uint32_t method_index = (uint32_t)reader_.readCell();
                const smx_rtti_method* method = smx_->GetMethod(method_index);
                if (op == LL_CALLN) {
                    uint8_t nargs = reader_.read<uint8_t>();
                    pushCell(nargs);
                } else {
                    auto parser = smx_->GetTypeParser(method->signature);
                    uint32_t arg_count;
                    if (!parser.ReadFunctionSignatureArgCount(&arg_count)) {
                        rt_->ReportErrorNumber(SP_ERROR_INSTRUCTION_PARAM);
                        return false;
                    }
                    pushCell(arg_count);
                }

                uint32_t native_index;
                cell_t result = 0;
                if (rt_->GetNativeIndex(method_index, &native_index)) {
                    cell_t* params = eval_stack_ptr_;

                    NativeEntry* native = rt_->NativeAt(native_index);
                    ivk_->enterNativeCall(native_index);
                    if (native->status == SP_NATIVE_BOUND) {
                        HeapSave save_hp(rt_->heap());
                        ke::SaveRestore<uint32_t> save_sp(rt_->sp());

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
                    {
                        // Update sp_ so that the callee can find its parameters.
                        auto updated_sp = rt_->heap().ToLocalAddr(eval_stack_ptr_);
                        ke::SaveAndSet<uint32_t> save_updated_sp(&rt_->sp(), updated_sp);
                        if (!Run(rt_, target, &result))
                            return false;
                    }
                }
                // An OP_CALL is always preceded by pushing the argument count.
                cell_t nparams = popCell();
                for (cell_t i = 0; i < nparams; i++)
                    popStack();

                if (!smx_->IsVoidMethod(method))
                    pushCell(result);
                break;
            }
            case LL_JUMP: {
                cell_t offset = reader_.readCell();
                if (offset < (cell_t)(insn_begin - ll_code)) {
                    if (!Environment::get()->watchdog()->HandleInterrupt()) {
                        rt_->ReportErrorNumber(SP_ERROR_TIMEOUT);
                        return false;
                    }
                }
                reader_.set_cursor(ll_code + offset);
                break;
            }
            case LL_JZER:
            case LL_JNZ:
            case LL_JEQ:
            case LL_JNEQ:
            case LL_JSLESS:
            case LL_JSLEQ:
            case LL_JSGRTR:
            case LL_JSGEQ: {
                cell_t offset = reader_.readCell();
                cell_t a, b;
                bool jump = false;
                switch (op) {
                    case LL_JZER:
                        a = popCell();
                        jump = a == 0;
                        break;
                    case LL_JNZ:
                        a = popCell();
                        jump = a != 0;
                        break;
                    case LL_JEQ:
                        b = popCell();
                        a = popCell();
                        jump = a == b;
                        break;
                    case LL_JNEQ:
                        b = popCell();
                        a = popCell();
                        jump = a != b;
                        break;
                    case LL_JSLESS:
                        b = popCell();
                        a = popCell();
                        jump = a < b;
                        break;
                    case LL_JSLEQ:
                        b = popCell();
                        a = popCell();
                        jump = a <= b;
                        break;
                    case LL_JSGRTR:
                        b = popCell();
                        a = popCell();
                        jump = a > b;
                        break;
                    case LL_JSGEQ:
                        b = popCell();
                        a = popCell();
                        jump = a >= b;
                        break;
                    default: assert(false);
                }
                if (jump) {
                    if (offset < (cell_t)(insn_begin - ll_code)) {
                        if (!Environment::get()->watchdog()->HandleInterrupt()) {
                            rt_->ReportErrorNumber(SP_ERROR_TIMEOUT);
                            return false;
                        }
                    }
                    reader_.set_cursor(ll_code + offset);
                }
                break;
            }
            case LL_SHL: {
                cell_t b = popCell();
                cell_t a = popCell();
                pushCell(a << b);
                break;
            }
            case LL_SHR: {
                cell_t right = popCell();
                cell_t left = popCell();
                pushCell(uint32_t(left) >> uint32_t(right));
                break;
            }
            case LL_SSHR: {
                cell_t b = popCell();
                cell_t a = popCell();
                pushCell(a >> b);
                break;
            }
            case LL_SMUL_I32: {
                cell_t b = popCell();
                cell_t a = popCell();
                pushCell(a * b);
                break;
            }
            case LL_SDIV_I32: {
                cell_t b = popCell();
                cell_t a = popCell();
                if (b == 0) {
                    rt_->ReportErrorNumber(SP_ERROR_DIVIDE_BY_ZERO);
                    return false;
                }
                if (b == -1 && a == cell_t(0x80000000)) {
                    rt_->ReportErrorNumber(SP_ERROR_INTEGER_OVERFLOW);
                    return false;
                }
                pushCell(a / b);
                break;
            }
            case LL_SMOD_I32: {
                cell_t b = popCell();
                cell_t a = popCell();
                if (b == 0) {
                    rt_->ReportErrorNumber(SP_ERROR_DIVIDE_BY_ZERO);
                    return false;
                }
                if (b == -1 && a == cell_t(0x80000000)) {
                    rt_->ReportErrorNumber(SP_ERROR_INTEGER_OVERFLOW);
                    return false;
                }
                pushCell(a % b);
                break;
            }
            case LL_ADD_I32: {
                cell_t b = popCell();
                cell_t a = popCell();
                pushCell(a + b);
                break;
            }
            case LL_SUB_I32: {
                cell_t b = popCell();
                cell_t a = popCell();
                pushCell(a - b);
                break;
            }
            case LL_AND: {
                cell_t b = popCell();
                cell_t a = popCell();
                pushCell(a & b);
                break;
            }
            case LL_OR: {
                cell_t b = popCell();
                cell_t a = popCell();
                pushCell(a | b);
                break;
            }
            case LL_XOR: {
                cell_t b = popCell();
                cell_t a = popCell();
                pushCell(a ^ b);
                break;
            }
            case LL_NOT: {
                cell_t val = popCell();
                pushCell(val ? 0 : 1);
                break;
            }
            case LL_NEG: {
                cell_t val = popCell();
                pushCell(-val);
                break;
            }
            case LL_INVERT: {
                cell_t val = popCell();
                pushCell(~val);
                break;
            }

            case LL_EQ_I32:
            case LL_NEQ_I32:
            case LL_SLESS_I32:
            case LL_SLEQ_I32:
            case LL_SGRTR_I32:
            case LL_SGEQ_I32: {
                cell_t b = popCell();
                cell_t a = popCell();
                cell_t result = 0;
                switch (op) {
                    case LL_SGRTR_I32: result = (a > b) ? 1 : 0; break;
                    case LL_SGEQ_I32:  result = (a >= b) ? 1 : 0; break;
                    case LL_SLEQ_I32:  result = (a <= b) ? 1 : 0; break;
                    case LL_SLESS_I32: result = (a < b) ? 1 : 0; break;
                    case LL_EQ_I32:    result = (a == b) ? 1 : 0; break;
                    case LL_NEQ_I32:   result = (a != b) ? 1 : 0; break;
                    default: assert(false);
                }
                pushCell(result);
                break;
            }
            case LL_EQ_I64:
            case LL_NEQ_I64:
            case LL_SLESS_I64:
            case LL_SLEQ_I64:
            case LL_SGRTR_I64:
            case LL_SGEQ_I64: {
                int64_t b = popInt64();
                int64_t a = popInt64();
                cell_t result = 0;
                switch (op) {
                    case LL_SGRTR_I64: result = (a > b) ? 1 : 0; break;
                    case LL_SGEQ_I64:  result = (a >= b) ? 1 : 0; break;
                    case LL_SLEQ_I64:  result = (a <= b) ? 1 : 0; break;
                    case LL_SLESS_I64: result = (a < b) ? 1 : 0; break;
                    case LL_EQ_I64:    result = (a == b) ? 1 : 0; break;
                    case LL_NEQ_I64:   result = (a != b) ? 1 : 0; break;
                    default: assert(false);
                }
                pushCell(result);
                break;
            }
            case LL_TEST_F32: {
                cell_t val = popCell();
                FloatCellUnion f(val);
                pushCell((f.f32 && !ke::IsNaN(f.f32)) ? 1 : 0);
                break;
            }
            case LL_NEG_F32: {
                cell_t val = popCell();
                FloatCellUnion f(val);
                pushCell(FloatCellUnion(-f.f32).cell);
                break;
            }
            case LL_MUL_F32: {
                cell_t b_val = popCell();
                cell_t a_val = popCell();
                FloatCellUnion a(a_val);
                FloatCellUnion b(b_val);
                pushCell(FloatCellUnion(a.f32 * b.f32).cell);
                break;
            }
            case LL_DIV_F32: {
                cell_t b_val = popCell();
                cell_t a_val = popCell();
                FloatCellUnion a(a_val);
                FloatCellUnion b(b_val);
                pushCell(FloatCellUnion(a.f32 / b.f32).cell);
                break;
            }
            case LL_MOD_F32: {
                cell_t b_val = popCell();
                cell_t a_val = popCell();
                FloatCellUnion a(a_val);
                FloatCellUnion b(b_val);
                pushCell(FloatCellUnion(fmodf(a.f32, b.f32)).cell);
                break;
            }
            case LL_ADD_F32: {
                cell_t b_val = popCell();
                cell_t a_val = popCell();
                FloatCellUnion a(a_val);
                FloatCellUnion b(b_val);
                pushCell(FloatCellUnion(a.f32 + b.f32).cell);
                break;
            }
            case LL_SUB_F32: {
                cell_t b_val = popCell();
                cell_t a_val = popCell();
                FloatCellUnion a(a_val);
                FloatCellUnion b(b_val);
                pushCell(FloatCellUnion(a.f32 - b.f32).cell);
                break;
            }
            case LL_EQ_F32:
            case LL_NEQ_F32:
            case LL_LESS_F32:
            case LL_LEQ_F32:
            case LL_GRTR_F32:
            case LL_GEQ_F32: {
                cell_t b_val = popCell();
                cell_t a_val = popCell();
                FloatCellUnion a(a_val);
                FloatCellUnion b(b_val);
                cell_t result = 0;
                switch (op) {
                    case LL_GRTR_F32: result = (a.f32 > b.f32); break;
                    case LL_GEQ_F32:  result = (a.f32 >= b.f32); break;
                    case LL_LEQ_F32:  result = (a.f32 <= b.f32); break;
                    case LL_LESS_F32: result = (a.f32 < b.f32); break;
                    case LL_EQ_F32:    result = (a.f32 == b.f32); break;
                    case LL_NEQ_F32:   result = (a.f32 != b.f32); break;
                    default: assert(false);
                }
                pushCell(result);
                break;
            }
            case LL_CVT_F32: {
                cell_t val = popCell();
                pushCell(FloatCellUnion((float)val).cell);
                break;
            }
            case LL_INC: {
                cell_t val = popCell();
                pushCell(val + 1);
                break;
            }
            case LL_DEC: {
                cell_t val = popCell();
                pushCell(val - 1);
                break;
            }
            case LL_COPYARRAY: {
                SpArray* src = heap_.ToPhysAddr<SpArray*>(popCell());
                SpArray* dest = heap_.ToPhysAddr<SpArray*>(popCell());
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
                cell_t src_addr = popCell();
                cell_t dest_addr = popCell();
                uint8_t* dest = heap_.ToPhysAddr<uint8_t*>(dest_addr);
                uint8_t* src = heap_.ToPhysAddr<uint8_t*>(src_addr);
                memcpy(dest, src, bytes);
                break;
            }
            case LL_ADDR_S: {
                cell_t slot = reader_.readInt16();
                cell_t address = frm_ + StackOffset(slot);
                pushCell(address);
                break;
            }
            case LL_SWITCH: {
                cell_t ncases = reader_.readCell();
                cell_t defaultOffset = reader_.readCell();
                cell_t val = popCell();
                cell_t jumpOffset = defaultOffset;
                for (cell_t i = 0; i < ncases; i++) {
                    cell_t case_val = reader_.readCell();
                    cell_t case_offset = reader_.readCell();
                    if (case_val == val) {
                        jumpOffset = case_offset;
                    }
                }
                reader_.set_cursor(ll_code + jumpOffset);
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
            case LL_NEWARRAY: {
                uint32_t type_id = reader_.read<uint32_t>();
                auto td = rt_->LoadTypeFromId(type_id);
                uint32_t size;
                if (td->kind() == TypeKind::Array) {
                    size = popCell();
                    if (size < 0) {
                        rt_->ReportErrorNumber(SP_ERROR_ARRAY_BOUNDS);
                        return false;
                    }
                } else {
                    size = td->array_size();
                }
                auto array = rt_->NewArray(td, size);
                if (!array)
                    return false;
                pushCell(rt_->heap().ToLocalAddr(array));
                break;
            }
            case LL_NEWBULKARRAY: {
                uint8_t dims = reader_.read<uint8_t>();
                uint32_t type_id = reader_.read<uint32_t>();
                auto td = rt_->LoadTypeFromId(type_id);
                auto array = rt_->NewBulkArray(td, dims, eval_stack_ptr_);
                if (!array)
                    return false;
                for (size_t i = 0; i < dims; i++)
                    popStack();
                pushCell(rt_->heap().ToLocalAddr(array));
                break;
            }
            case LL_FILLARRAY: {
                uint32_t data_offset = reader_.readCell();

                uint32_t addr = popCell();
                auto array = rt_->heap().ToPhysAddr<SpArray*>(addr);
                if (!rt_->FillArray(array, data_offset))
                    return false;
                break;
            }
            case LL_ARRAY_TO_NATIVE: {
                uint32_t addr = popCell();
                assert((addr & kNativePointerTag) == 0);

                pushCell(addr | kNativePointerTag);
                break;
            }
            case LL_SLICE_ES: {
                uint32_t cell_count = reader_.read<uint32_t>();
                cell_t base = popCell();
                SpArray* slice = rt_->heap().AllocTyped<SpArray>();
                if (!slice)
                    return false;
                slice->td = rt_->GetSliceType(rt_->GetPrimitiveType(TypeKind::Any));
                slice->length = cell_count;
                slice->data = base;
                pushCell(rt_->heap().ToLocalAddr(slice));
                break;
            }

            default:
                assert(false);
                return false;
        }
    }

    return true;
}

cell_t Interpreter::StackOffset(cell_t slot) {
    if (slot < 0) {
        // -1 is because we can't encode 0-based arguments, because 0 is local.
        // +1 because we skip the argument count.
        return (-slot - 1 + 1) * sizeof(cell_t);
    }

    return method_->local_offsets().at(slot);
}

cell_t Interpreter::getLocalCell(int32_t slot) {
    return phys_frm_[StackOffset(slot) / sizeof(cell_t)];
}

void Interpreter::setLocalCell(int32_t slot, cell_t value) {
    phys_frm_[StackOffset(slot) / sizeof(cell_t)] = value;
}

int64_t& Interpreter::getLocalInt64(int32_t slot) {
    return *reinterpret_cast<int64_t*>(&phys_frm_[StackOffset(slot) / sizeof(cell_t)]);
}

void Interpreter::pushCell(cell_t value) {
    assert(stack_types_ptr_ > stack_types_limit_);
    assert(eval_stack_ptr_ > eval_stack_limit_);
    *--stack_types_ptr_ = (uint8_t)StackType::Cell;
    *--eval_stack_ptr_ = value;
}

cell_t Interpreter::popCell() {
    assert(stack_types_ptr_ < stack_types_top_);
    assert((StackType)*stack_types_ptr_ == StackType::Cell);
    assert(eval_stack_ptr_ < eval_stack_top_);
    stack_types_ptr_++;
    return *eval_stack_ptr_++;
}

void Interpreter::pushInt64(int64_t value) {
    assert(stack_types_ptr_ > stack_types_limit_);
    assert(eval_stack_ptr_ - 2 >= eval_stack_limit_);
    *--stack_types_ptr_ = (uint8_t)StackType::Int64;
    eval_stack_ptr_ -= 2;
    *reinterpret_cast<int64_t*>(eval_stack_ptr_) = value;
}

int64_t Interpreter::popInt64() {
    assert(stack_types_ptr_ < stack_types_top_);
    assert((StackType)*stack_types_ptr_ == StackType::Int64);
    assert(eval_stack_ptr_ + 2 <= eval_stack_top_);
    stack_types_ptr_++;
    int64_t val = *reinterpret_cast<int64_t*>(eval_stack_ptr_);
    eval_stack_ptr_ += 2;
    return val;
}

void Interpreter::popStack() {
    assert(stack_types_ptr_ < stack_types_top_);
    StackType type = (StackType)*(stack_types_ptr_++);
    if (type == StackType::Cell) {
        assert(eval_stack_ptr_ < eval_stack_top_);
        eval_stack_ptr_++;
    } else {
        assert(eval_stack_ptr_ + 2 <= eval_stack_top_);
        eval_stack_ptr_ += 2;
    }
}

Interpreter::StackValue Interpreter::popValue() {
    assert(stack_types_ptr_ < stack_types_top_);
    StackValue v;
    v.type = (StackType)*stack_types_ptr_;
    if (v.type == StackType::Cell) {
        v.u.cell = popCell();
    } else {
        v.u.i64 = popInt64();
    }
    return v;
}

void Interpreter::pushValue(const StackValue& v) {
    if (v.type == StackType::Cell)
        pushCell(v.u.cell);
    else if (v.type == StackType::Int64)
        pushInt64(v.u.i64);
}

int32_t Interpreter::CalcLocalsSize() {
    if (!method_->local_offsets().empty())
        return -method_->local_offsets().back();

    if (method_->local_types().empty())
        return 0;

    // Cache this calculation for future runs.
    method_->local_offsets() = ke::FixedArray<int32_t>(method_->local_types().size());

    int32_t size = 0;
    for (size_t i = 0; i < method_->local_types().size(); i++) {
        auto td = method_->local_types().at(i);

        size += td->slot_size();
        method_->local_offsets().at(i) = -size;
    }
    return size;
}

bool Interpreter::InitLocals() {
    for (size_t i = 0; i < method_->local_types().size(); i++) {
        auto td = method_->local_types().at(i);
        if (td->kind() == TypeKind::FixedArray) {
            auto array = rt_->NewArray(td, td->array_size());
            if (!array)
                return false;
            setLocalCell(i, rt_->heap().ToLocalAddr(array));
        }
    }
    return true;
}

} // namespace sp::v2
