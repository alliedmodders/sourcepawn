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

#include <amtl/am-string.h>

#include "vm/binary-reader.h"
#include "vm/smx-image.h"

using namespace ke;
using namespace sp;
using namespace SourcePawn;

std::string DumpTool::EscapeString(std::string_view s) {
    std::string res;
    for (char c : s) {
        unsigned char uc = (unsigned char)c;
        if (uc == '\"') {
            res += "\\\"";
        } else if (uc == '\\') {
            res += "\\\\";
        } else if (uc == '\n') {
            res += "\\n";
        } else if (uc == '\r') {
            res += "\\r";
        } else if (uc == '\t') {
            res += "\\t";
        } else if (uc >= 32 && uc <= 126) {
            res += c;
        } else {
            char buf[8];
            snprintf(buf, sizeof(buf), "\\x%02x", uc);
            res += buf;
        }
    }
    return res;
}

std::string DumpTool::DumpString(uint16_t index) {
    auto table = smx()->rtti_stringpool();
    if (!table || index >= table->row_count)
        return "unknown_string_" + std::to_string(index);

    auto entry = smx()->getRttiRow<smx_rtti_string>(table, index);
    auto blob = smx()->ReadDataBlob(entry->offset);
    if (!blob)
        return std::string("<invalid_blob_0x") + ke::Sprintf("%x", entry->offset).get() + ">";

    std::string_view s = *blob;
    std::string res = "\"";
    if (s.length() > 60) {
        res += EscapeString(s.substr(0, 57));
        res += "...\"";
    } else {
        res += EscapeString(s);
        res += "\"";
    }
    return res;
}

void DumpTool::DumpCodeRangeV2(uint32_t pcode_start, uint32_t pcode_end) {
    using namespace sp::v2;
    auto code = smx_->DescribeCode();

    auto start = code.bytes + pcode_start;
    auto cip = start;
    auto code_end = code.bytes + pcode_end;
    auto method_start = cip;

    while (cip < code_end) {
        OPCODE op = (OPCODE)*cip;

        const char* name = nullptr;
        if (op < OPCODES_LAST)
            name = GetOpcodeName(op);

        uint32_t line;
        uint32_t offset = uint32_t(cip - code.bytes);
        if (smx_->IsLineBoundary(offset) && smx_->LookupLine(offset, &line)) {
            if (cip != method_start)
                fprintf(stdout, "\n");
            fprintf(stdout, "    ; line %u\n", line);
        } else if (cip != method_start) {
            fprintf(stdout, "\n");
        }

        fprintf(stdout, "    %04x: ", (uint32_t)(cip - method_start));
        if (name)
            fprintf(stdout, "%s", name);
        else
            fprintf(stdout, "unknown_op_%u", op);

        DumpOpcodeV2(method_start, cip, op);

        if (op == OP_SWITCH)
            cip += GetSwitchOpcodeSize(cip);
        else if (name)
            cip += GetOpcodeSize(op);
        else
            cip++;
    }
    fprintf(stdout, "\n");
}

void DumpTool::DumpOpcodeV2(const uint8_t* method_start, const uint8_t* cip, sp::v2::OPCODE op) {
    using namespace sp::v2;
    BinaryReader reader(cip + 1);

    switch (op) {
        case OP_PUSH_C:
            fprintf(stdout, " %d", reader.read<cell_t>());
            break;

        case OP_LOAD_GLB:
        case OP_STOR_GLB:
        case OP_ADDR_GLB:
        {
            uint16_t index = reader.read<uint16_t>();
            auto globals = smx_->rtti_globals();
            if (globals && index < globals->row_count) {
                auto global = smx_->getRttiRow<smx_rtti_global>(globals, index);
                fprintf(stdout, " %s", smx_->names() + global->name);
            } else {
                fprintf(stdout, " unknown_global_%u", index);
            }
            break;
        }

        case OP_LOAD_STR:
            fprintf(stdout, " %s", DumpString(reader.read<uint16_t>()).c_str());
            break;

        case OP_PUSH_C_I8:
            fprintf(stdout, " %d", (int)reader.read<int8_t>());
            break;

        case OP_PUSH_C_I64:
            fprintf(stdout, " %" PRId64, reader.read<int64_t>());
            break;

        case OP_PUSH_C_F32:
            fprintf(stdout, " %f", reader.read<float>());
            break;

        case OP_ADDR_S:
        case OP_LOAD_S:
        case OP_STOR_S:
            fprintf(stdout, " %d", reader.read<int16_t>());
            break;

        case OP_STOR_S_C: {
            int16_t offset = reader.read<int16_t>();
            cell_t value = reader.read<cell_t>();
            fprintf(stdout, " %d, %d", offset, value);
            break;
        }

        case OP_LOAD_FN:
        case OP_CALL:
        case OP_CALLN:
        case OP_CALLVA:
        {
            uint32_t table_id = reader.read<uint32_t>();
            uint32_t selector = GetTableIdSelector(table_id);
            uint32_t method_index = GetTableIdIndex(table_id);
            if (selector == kTableId_RttiMethod) {
                if (auto method = smx_->GetMethod(method_index))
                    fprintf(stdout, " %s", smx_->names() + method->name);
                else
                    fprintf(stdout, " unknown_method_%u", method_index);
            } else {
                fprintf(stdout, " table_id_%u", table_id);
            }

            if (op == OP_CALLN || op == OP_CALLVA) {
                uint8_t nargs = reader.read<uint8_t>();
                fprintf(stdout, " %u", nargs);
            }
            break;
        }

        case OP_JUMP:
        case OP_JZER:
        case OP_JNZ:
        case OP_JEQ:
        case OP_JNEQ:
        case OP_JSLESS:
        case OP_JSGRTR:
        case OP_JSGEQ:
        case OP_JSLEQ:
        {
            uint32_t target_offs = reader.read<uint32_t>();
            uint32_t diff = target_offs - (uint32_t)(method_start - smx_->DescribeCode().bytes);
            fprintf(stdout, " %04x ; %x", diff, target_offs);
            break;
        }

        case OP_NEWARRAY: {
            uint32_t type_id = reader.read<uint32_t>();
            auto rtti = smx_->GetTypeIdParser(type_id);
            fprintf(stdout, " %s", DumpType(rtti).c_str());
            break;
        }

        case OP_LOAD_ES_SIZE: {
            uint32_t type_id = reader.read<uint32_t>();
            auto rtti = smx_->GetTypeIdParser(type_id);
            fprintf(stdout, " %s", DumpType(rtti).c_str());
            break;
        }

        case OP_COPYOBJ: {
            uint32_t type_id = reader.read<uint32_t>();
            auto rtti = smx_->GetTypeIdParser(type_id);
            fprintf(stdout, " %s", DumpType(rtti).c_str());
            break;
        }

        case OP_SLICE_AS:
        case OP_SLICE_ES: {
            uint32_t type_id = reader.read<uint32_t>();
            auto rtti = smx_->GetTypeIdParser(type_id);
            fprintf(stdout, " %s", DumpType(rtti).c_str());
            break;
        }

        case OP_FILLARRAY: {
            uint32_t data_offs = reader.read<uint32_t>();
            fprintf(stdout, " 0x%x", data_offs);
            break;
        }

        case OP_NEWOBJ: {
            uint32_t operand = reader.read<uint32_t>();
            uint32_t classdef_index = operand >> 1;
            if (auto cls = smx_->getClassdef(classdef_index))
                fprintf(stdout, " %s", smx_->names() + cls->name);
            else
                fprintf(stdout, " unknown_class_%u", classdef_index);
            break;
        }

        case OP_LOAD_FLD:
        case OP_ADDR_FLD:
        case OP_STOR_FLD:
        case OP_LOAD_FLD_OFFSET:
        {
            uint32_t table_id = reader.read<uint32_t>();
            if (GetTableIdSelector(table_id) == kTableId_RttiField) {
                uint32_t field_index = GetTableIdIndex(table_id);
                const char* cls_name = "unknown";
                const char* field_name = "unknown";
                if (auto cls = smx_->FindClassdefForField(field_index))
                    cls_name = smx_->names() + cls->name;
                if (auto field = smx_->getField(field_index))
                    field_name = smx_->names() + field->name;
                fprintf(stdout, " %s::%s", cls_name, field_name);
            } else {
                fprintf(stdout, " unknown_field_ref_%u", table_id);
            }
            break;
        }

        case OP_LOAD_UPVAR:
        case OP_STOR_UPVAR:
        case OP_ADDR_UPVAR:
            fprintf(stdout, " %u", reader.read<uint16_t>());
            break;

        case OP_NEWCLOSURE:
        {
            uint32_t method_index = reader.read<uint32_t>();
            if (auto method = smx_->GetMethod(method_index))
                fprintf(stdout, " %s", method->name ? smx_->names() + method->name : "unknown");
            else
                fprintf(stdout, " unknown_method_%u", method_index);
            break;
        }

        case OP_POP:
        case OP_DUP:
        case OP_SWAP:
            break;

        default:
            break;
    }
}
