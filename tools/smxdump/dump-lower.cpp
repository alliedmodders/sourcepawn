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

#include <inttypes.h>

#include <string>
#include <vector>

#include <amtl/am-string.h>
#include "v2/lowering/ll-op.h"
#include "v2/lowering/llcode.h"
#include "v2/lowering/lowering.h"
#include "v2/method-info.h"
#include "v2/runtime.h"
#include "vm/binary-reader.h"
#include "vm/smx-image.h"
#include "vm/type-desc.h"

using namespace ke;
using namespace sp;
using namespace sp::v2;
using namespace SourcePawn;

void DumpTool::DumpLoweredCode(uint32_t method_index) {
    ke::RefPtr<MethodInfo> method = runtime_->AcquireMethod(method_index);
    if (!method->llcode()) {
        ke::RefPtr<ControlFlowGraph> graph = method->BuildGraph();
        if (!graph) {
            fprintf(stdout, "    <failed to build graph for lowered code>\n");
            return;
        }
        std::unique_ptr<LLCode> code = LowerMethod(graph, method.get());
        method->set_llcode(std::move(code));
    }

    const uint8_t* ll_code = method->llcode()->bytes();
    size_t ll_size = method->llcode()->size();
    const uint8_t* cip = ll_code;
    const uint8_t* code_end = ll_code + ll_size;

    uint32_t argc = method->arg_types().size();
    uint32_t total_regs = argc;
    for (size_t i = 0; i < method->local_types().size(); i++)
        total_regs += method->local_types()[i]->slot_size() / sizeof(cell_t);

    auto FormatRegister = [&](uint16_t reg) -> std::string {
        if (reg == 0xffff)
            return "none";
        if (reg < argc)
            return "a" + std::to_string(reg);
        if (reg < total_regs)
            return "r" + std::to_string(reg - argc);
        return "v" + std::to_string(reg);
    };

    while (cip < code_end) {
        uint32_t ll_offset = (uint32_t)(cip - ll_code);
        uint32_t high_offset = method->llcode()->LookupHighOffset(ll_offset);

        uint32_t line;
        if (smx()->IsLineBoundary(high_offset) && smx()->LookupLine(high_offset, &line)) {
            fprintf(stdout, "    ; line %u\n", line);
        }

        LLOp op = (LLOp)*reinterpret_cast<const uint16_t*>(cip);
        const char* op_name = GetLLOpName(op);
        fprintf(stdout, "    %04x: %s", ll_offset, op_name);

        BinaryReader reader(cip + 2);

        switch (op) {
            case LL_SWITCH: {
                cell_t ncases = reader.read<cell_t>();
                fprintf(stdout, " cases:%d", (int)ncases);
                cell_t def_offset = reader.read<cell_t>();
                for (cell_t c = 0; c < ncases; c++) {
                    cell_t val = reader.read<cell_t>();
                    cell_t target_offset = reader.read<cell_t>();
                    fprintf(stdout, "\n        case %d: %04x", (int)val, (uint32_t)target_offset);
                }
                fprintf(stdout, "\n        default: %04x", (uint32_t)def_offset);
                break;
            }

            case LL_CALL: {
                auto method = reader.read<const smx_rtti_method*>();
                uint8_t nargs = reader.read<uint8_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                std::vector<std::string> args;
                args.push_back(smx()->names() + method->name);
                args.push_back(std::to_string(nargs));
                if (dest_reg != 0xffff)
                    args.push_back("dest:" + FormatRegister(dest_reg));
                for (uint32_t i = 0; i < nargs; i++)
                    args.push_back(FormatRegister(reader.read<uint16_t>()));
                fprintf(stdout, " %s", ke::Join(args, ", ").c_str());
                break;
            }

            case LL_NTVCALL: {
                uint32_t native_index = reader.read<uint32_t>();
                uint8_t nargs = reader.read<uint8_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                std::vector<std::string> args;
                args.push_back(runtime_->GetNative(native_index)->name);
                args.push_back(std::to_string(nargs));
                if (dest_reg != 0xffff)
                    args.push_back("dest:" + FormatRegister(dest_reg));
                for (uint32_t i = 0; i < nargs; i++)
                    args.push_back(FormatRegister(reader.read<uint16_t>()));
                fprintf(stdout, " %s", ke::Join(args, ", ").c_str());
                break;
            }

            case LL_NTVCALL_VA: {
                uint32_t native_index = reader.read<uint32_t>();
                uint8_t nargs = reader.read<uint8_t>();
                uint16_t spread_reg = reader.read<uint16_t>();
                uint16_t dest_reg = reader.read<uint16_t>();
                std::vector<std::string> args;
                args.push_back(runtime_->GetNative(native_index)->name);
                args.push_back(std::to_string(nargs));
                args.push_back("spread:" + FormatRegister(spread_reg));
                if (dest_reg != 0xffff)
                    args.push_back("dest:" + FormatRegister(dest_reg));
                for (uint32_t i = 0; i < nargs; i++)
                    args.push_back(FormatRegister(reader.read<uint16_t>()));
                fprintf(stdout, " %s", ke::Join(args, ", ").c_str());
                break;
            }

            default: {
                const LLArgFmt* args = nullptr;
                size_t nargs = 0;

                switch (op) {
#define FOR_EACH_OPCODE(op_name, val, text, ...) \
                case LL_##op_name: { \
                    static const LLArgFmt fmt[] = __VA_ARGS__; \
                    args = fmt; \
                    nargs = sizeof(fmt) / sizeof(LLArgFmt); \
                    break; \
                }
                LL_OPCODE_LIST(FOR_EACH_OPCODE)
#undef FOR_EACH_OPCODE
                }

                std::vector<std::string> op_args;
                for (size_t i = 0; i < nargs; i++) {
                    switch (args[i]) {
                        case LL_FMT_REG:
                            op_args.push_back(FormatRegister(reader.read<uint16_t>()));
                            break;
                        case LL_FMT_TYPEDESC: {
                            auto td = reader.read<const TypeDesc*>();
                            op_args.push_back(DumpType(td));
                            break;
                        }
                        case LL_FMT_METHOD_PTR: {
                            auto method = reader.read<const smx_rtti_method*>();
                            op_args.push_back(smx()->names() + method->name);
                            break;
                        }
                        case LL_FMT_METHOD_ID: {
                            uint32_t method_index = reader.read<uint32_t>();
                            auto method = smx()->GetMethod(method_index);
                            op_args.push_back(smx()->names() + method->name);
                            break;
                        }
                        case LL_FMT_STR_ID:
                            op_args.push_back(DumpString(reader.read<uint16_t>()));
                            break;
                        case LL_FMT_U8:
                            op_args.push_back(std::to_string(reader.read<uint8_t>()));
                            break;
                        case LL_FMT_U16:
                            op_args.push_back(std::to_string(reader.read<uint16_t>()));
                            break;
                        case LL_FMT_GLB_ID: {
                            uint16_t index = reader.read<uint16_t>();
                            auto globals = smx()->rtti_globals();
                            auto global = smx()->getRttiRow<smx_rtti_global>(globals, index);
                            op_args.push_back(smx()->names() + global->name);
                            break;
                        }
                        case LL_FMT_I32:
                            op_args.push_back(std::to_string(reader.read<int32_t>()));
                            break;
                        case LL_FMT_U32:
                            op_args.push_back(std::to_string(reader.read<uint32_t>()));
                            break;
                        case LL_FMT_CELL:
                            op_args.push_back(std::to_string(reader.read<cell_t>()));
                            break;
                        case LL_FMT_I64:
                            op_args.push_back(std::to_string(reader.read<int64_t>()));
                            break;
                        case LL_FMT_TARGET: {
                            cell_t target_offset = reader.read<cell_t>();
                            char buf[16];
                            snprintf(buf, sizeof(buf), "%04x", (uint32_t)target_offset);
                            op_args.push_back(buf);
                            break;
                        }
                        case LL_FMT_SWITCH:
                        case LL_FMT_CALL:
                            break;
                    }
                }

                if (!op_args.empty())
                    fprintf(stdout, " %s", ke::Join(op_args, ", ").c_str());
                break;
            }
        }

        fprintf(stdout, "\n");
        cip = reader.cursor();
    }
}

std::string DumpTool::DumpType(const TypeDesc* td) {
    if (!td)
        return "null";
    switch (td->kind()) {
        case TypeKind::Bool:
            return "bool";
        case TypeKind::Int32:
            return "int";
        case TypeKind::Int64:
            return "int64";
        case TypeKind::Float32:
            return "float";
        case TypeKind::Char8:
            return "char";
        case TypeKind::Any:
            return "any";
        case TypeKind::Void:
            return "void";
        case TypeKind::TopFunction:
            return "function";
        case TypeKind::Array:
            return DumpType(td->array_elt()) + "[]";
        case TypeKind::FixedArray:
            return DumpType(td->array_elt()) + "[" + std::to_string(td->array_size()) + "]";
        case TypeKind::FlatArray:
            return DumpType(td->array_elt()) + "[flat:" + std::to_string(td->array_size()) + "]";
        case TypeKind::ArraySlice:
            return DumpType(td->array_elt()) + "[slice]";
        case TypeKind::Reference:
            return DumpType(td->ref_type()) + "&";
        case TypeKind::EnumStruct:
            if (td->HasClassdef() && td->cls())
                return std::string("enum_struct ") + (smx()->names() + td->cls()->name);
            return "enum_struct";
        default:
            return "unknown";
    }
}
