// vim: set sts=4 ts=8 sw=4 tw=99 et:
// 
// Copyright (C) 2026 AlliedModders LLC
// 
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#include "smxdump.h"
#include "vm/smx-image.h"
#include "v2/runtime.h"
#include "v2/method-info.h"
#include "v2/interp/lowering.h"
#include "v2/interp/interp-code.h"
#include "v2/interp/ll-op.h"
#include "vm/binary-reader.h"
#include <inttypes.h>

using namespace ke;
using namespace sp;
using namespace sp::v2;
using namespace SourcePawn;

void DumpTool::DumpLoweredCode(uint32_t method_index) {
    ke::RefPtr<MethodInfo> method = runtime_->AcquireMethod(method_index);
    if (!method->interp()) {
        ke::RefPtr<ControlFlowGraph> graph = method->BuildGraph();
        if (!graph) {
            fprintf(stdout, "    <failed to build graph for lowered code>\n");
            return;
        }
        std::unique_ptr<InterpCode> code = LowerMethod(graph, method.get());
        method->setInterpCode(std::move(code));
    }

    const uint8_t* ll_code = method->interp()->bytes();
    size_t ll_size = method->interp()->size();
    const uint8_t* cip = ll_code;
    const uint8_t* code_end = ll_code + ll_size;

    while (cip < code_end) {
        uint32_t ll_offset = (uint32_t)(cip - ll_code);
        uint32_t high_offset = method->interp()->LookupHighOffset(ll_offset);

        uint32_t line;
        if (smx()->IsLineBoundary(high_offset) && smx()->LookupLine(high_offset, &line)) {
            fprintf(stdout, "    ; line %u\n", line);
        }

        LLOp op = (LLOp)*reinterpret_cast<const uint16_t*>(cip);
        const char* op_name = GetLLOpName(op);
        fprintf(stdout, "    %04x: %s", ll_offset, op_name);

        BinaryReader reader(cip + 2);

        switch (op) {
            case LL_NOP:
            case LL_POP:
            case LL_DUP:
            case LL_RETN:
            case LL_SHL:
            case LL_SHR:
            case LL_SSHR:
            case LL_SMUL_I32:
            case LL_SDIV_I32:
            case LL_SMOD_I32:
            case LL_ADD_I32:
            case LL_SUB_I32:
            case LL_AND:
            case LL_OR:
            case LL_XOR:
            case LL_NOT:
            case LL_NEG:
            case LL_INVERT:
            case LL_EQ_I32:
            case LL_NEQ_I32:
            case LL_SLESS_I32:
            case LL_SLEQ_I32:
            case LL_SGRTR_I32:
            case LL_SGEQ_I32:
            case LL_INC:
            case LL_DEC:
            case LL_HEAP_SAVE:
            case LL_HEAP_RESTORE:
            case LL_TEST_F32:
            case LL_NEG_F32:
            case LL_MUL_F32:
            case LL_DIV_F32:
            case LL_ADD_F32:
            case LL_SUB_F32:
            case LL_EQ_F32:
            case LL_NEQ_F32:
            case LL_LESS_F32:
            case LL_LEQ_F32:
            case LL_GRTR_F32:
            case LL_GEQ_F32:
            case LL_CVT_F32:
            case LL_MOD_F32:
            case LL_CVT_I64:
            case LL_TRUNCATE_I64:
            case LL_TEST_I64:
            case LL_INVERT_I64:
            case LL_NEG_I64:
            case LL_SMUL_I64:
            case LL_SDIV_I64:
            case LL_ADD_I64:
            case LL_SUB_I64:
            case LL_SHL_I64:
            case LL_SSHR_I64:
            case LL_SHR_I64:
            case LL_EQ_I64:
            case LL_NEQ_I64:
            case LL_OR_I64:
            case LL_AND_I64:
            case LL_XOR_I64:
            case LL_SLESS_I64:
            case LL_SLEQ_I64:
            case LL_SGRTR_I64:
            case LL_SGEQ_I64:
            case LL_SMOD_I64:
            case LL_SWAP:
            case LL_RETV:
            case LL_ARRAY_TO_NATIVE:
            case LL_SLICE:
            case LL_LOAD_I_I32:
            case LL_LOAD_I_U8:
            case LL_LOAD_I_I64:
            case LL_LOAD_I_F32:
            case LL_STOR_I_I32:
            case LL_STOR_I_U8:
            case LL_STOR_I_I64:
            case LL_STOR_I_F32:
            case LL_LOAD_ELEM_I32:
            case LL_LOAD_ELEM_F32:
            case LL_LOAD_ELEM_I64:
            case LL_LOAD_ELEM_U8:
            case LL_LOAD_ELEM_A:
            case LL_STOR_ELEM_I32:
            case LL_STOR_ELEM_F32:
            case LL_STOR_ELEM_I64:
            case LL_STOR_ELEM_U8:
            case LL_COPYARRAY:
            case LL_ARRAY_TO_FLAT:
            case LL_COPYARRAY_FLAT:
            case LL_SLICE_FLAT:
                break;

            case LL_LOAD_GLB:
            case LL_STOR_GLB:
            case LL_ADDR_GLB: {
                uint16_t index = reader.read<uint16_t>();
                auto globals = smx()->rtti_globals();
                if (globals && index < globals->row_count) {
                    auto global = smx()->getRttiRow<smx_rtti_global>(globals, index);
                    fprintf(stdout, " %s", smx()->names() + global->name);
                } else {
                    fprintf(stdout, " unknown_global_%u", index);
                }
                break;
            }

            case LL_LOAD_S:
            case LL_STOR_S:
            case LL_ADDR_S:
                fprintf(stdout, " %d", reader.read<int16_t>());
                break;

            case LL_STOR_S_C: {
                int16_t offset = reader.read<int16_t>();
                cell_t value = reader.read<cell_t>();
                fprintf(stdout, " %d, %d", offset, value);
                break;
            }

            case LL_PUSH_C:
                fprintf(stdout, " %d", reader.read<cell_t>());
                break;

            case LL_PUSH_C_I8:
                fprintf(stdout, " %d", (int)reader.read<int8_t>());
                break;

            case LL_PUSH_C_I64:
                fprintf(stdout, " %" PRId64, reader.read<int64_t>());
                break;

            case LL_LOAD_FN: {
                uint32_t method_index = reader.read<uint32_t>();
                if (auto method = smx()->GetMethod(method_index))
                    fprintf(stdout, " %s", smx()->names() + method->name);
                else
                    fprintf(stdout, " unknown_method_%u", method_index);
                break;
            }

            case LL_CALL: {
                uint32_t method_index = reader.read<uint32_t>();
                if (auto method = smx()->GetMethod(method_index))
                    fprintf(stdout, " %s", smx()->names() + method->name);
                else
                    fprintf(stdout, " unknown_method_%u", method_index);
                break;
            }

            case LL_CALLN: {
                uint32_t method_index = reader.read<uint32_t>();
                uint8_t nargs = reader.read<uint8_t>();
                if (auto method = smx()->GetMethod(method_index))
                    fprintf(stdout, " %s %u", smx()->names() + method->name, nargs);
                else
                    fprintf(stdout, " unknown_method_%u %u", method_index, nargs);
                break;
            }

            case LL_JUMP:
            case LL_JZER:
            case LL_JNZ:
            case LL_JEQ:
            case LL_JNEQ:
            case LL_JSLESS:
            case LL_JSLEQ:
            case LL_JSGRTR:
            case LL_JSGEQ: {
                cell_t target_offset = reader.read<cell_t>();
                cell_t target_id = reader.read<cell_t>();
                fprintf(stdout, " %04x ; block %d", (uint32_t)target_offset, (int)target_id);
                break;
            }

            case LL_NEWARRAY: {
                uint32_t type_id = reader.read<uint32_t>();
                auto rtti = smx()->GetTypeIdParser(type_id);
                fprintf(stdout, " %s", DumpType(rtti).c_str());
                break;
            }

            case LL_NEWBULKARRAY: {
                uint8_t ndims = reader.read<uint8_t>();
                uint32_t type_id = reader.read<uint32_t>();
                auto rtti = smx()->GetTypeIdParser(type_id);
                fprintf(stdout, " dims:%u %s", ndims, DumpType(rtti).c_str());
                break;
            }

            case LL_FILLARRAY: {
                uint32_t data_offs = reader.read<uint32_t>();
                reader.read<const TypeDesc*>();
                fprintf(stdout, " 0x%x", data_offs);
                break;
            }

            case LL_FILLARRAY_FLAT: {
                uint32_t data_offs = reader.read<uint32_t>();
                fprintf(stdout, " 0x%x", data_offs);
                break;
            }

            case LL_LOAD_FLD_X32:
            case LL_LOAD_FLD_X64:
            case LL_ADDR_FLD:
            case LL_STOR_FLD_X32:
            case LL_STOR_FLD_X64: {
                uint32_t offset = reader.read<uint32_t>();
                fprintf(stdout, " offset:%u", offset);
                break;
            }

            case LL_IDXADDR:
            case LL_IDXADDR_FLAT: {
                uint32_t array_size = reader.read<uint32_t>();
                uint32_t elt_size = reader.read<uint32_t>();
                fprintf(stdout, " size:%u elt_size:%u", array_size, elt_size);
                break;
            }

            case LL_LOAD_STR:
                DumpString(reader.read<uint16_t>());
                break;

            case LL_STOR_ELEM_FLAT_I32:
            case LL_STOR_ELEM_FLAT_F32:
            case LL_STOR_ELEM_FLAT_I64:
            case LL_STOR_ELEM_FLAT_U8: {
                uint32_t array_size = reader.read<uint32_t>();
                uint32_t elt_size = reader.read<uint32_t>();
                fprintf(stdout, " size:%u elt_size:%u", array_size, elt_size);
                break;
            }

            case LL_SLICE_ES: {
                uint32_t cell_count = reader.read<uint32_t>();
                fprintf(stdout, " cells:%u", cell_count);
                break;
            }

            case LL_COPYOBJ: {
                uint32_t size = reader.read<uint32_t>();
                fprintf(stdout, " size:%u", size);
                break;
            }

            case LL_SWITCH: {
                cell_t ncases = reader.read<cell_t>();
                fprintf(stdout, " cases:%d", (int)ncases);
                for (cell_t c = 0; c < ncases; c++) {
                    cell_t val = reader.read<cell_t>();
                    cell_t target_offset = reader.read<cell_t>();
                    cell_t target_id = reader.read<cell_t>();
                    fprintf(stdout, "\n        case %d: %04x ; block %d", (int)val, (uint32_t)target_offset, (int)target_id);
                }
                cell_t def_offset = reader.read<cell_t>();
                cell_t def_id = reader.read<cell_t>();
                fprintf(stdout, "\n        default: %04x ; block %d", (uint32_t)def_offset, (int)def_id);
                break;
            }

            default:
                break;
        }

        fprintf(stdout, "\n");
        cip = reader.cursor();
    }
}
