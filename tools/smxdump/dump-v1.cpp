// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#include "smxdump.h"
#include "vm/smx-image.h"

using namespace ke;
using namespace sp;
using namespace SourcePawn;

template <bool SearchForMethods>
void DumpTool::DumpCodeRangeV1(cell_t pcode_start, cell_t pcode_end) {
    using namespace sp::v1;
    auto code = smx_->DescribeCode();

    auto start = reinterpret_cast<const cell_t*>(code.bytes + pcode_start);
    auto cip = start;
    auto code_end = reinterpret_cast<const cell_t*>(code.bytes + pcode_end);
    auto method_start = cip;

    while (cip < code_end) {
        OPCODE op = (OPCODE)*cip;

        if (SearchForMethods && (cip == start || op == OP_PROC)) {
            std::string method_name;
            uint32_t offset = (cip - (const cell_t*)code.bytes) * sizeof(cell_t);
            if (auto name = smx_->LookupFunction(offset))
                method_name = name;
            else
                method_name = ke::StringPrintf("unknown_method_%u", offset);

            if (cip != start)
                fprintf(stdout, "\n}\n");

            fprintf(stdout, ".method %s\n", method_name.c_str());
            fprintf(stdout, "{\n");
            fprintf(stdout, "    .pcode_start = 0x%x\n", offset);

            method_start = cip;
        }

        const char* name = nullptr;
        if (op < OPCODES_LAST)
            name = GetOpcodeName(op);

        // Terminate previous line.
        if (cip != method_start)
            fprintf(stdout, "\n");

        fprintf(stdout, "    %04x: ", (uint32_t)((cip - method_start) * sizeof(cell_t)));
        if (name)
            fprintf(stdout, "%s", name);
        else
            fprintf(stdout, "unknown_op_%u", op);

        DumpOpcodeV1(method_start, cip, op);

        if (op == OP_CASETBL)
            cip += GetCaseTableSize(reinterpret_cast<const uint8_t*>(cip));
        else if (name)
            cip += GetOpcodeSize(op);
        else
            cip++;
    }
    if (SearchForMethods)
        fprintf(stdout, "\n}\n");
    fprintf(stdout, "\n");
}

void DumpTool::DumpOpcodeV1(const cell_t* method_start, const cell_t* cip, sp::v1::OPCODE op) {
    using namespace sp::v1;
    switch (op) {
        case OP_PUSH_C:
        case OP_PUSH_ADR:
        case OP_SHL_C_PRI:
        case OP_SHL_C_ALT:
        case OP_ADD_C:
        case OP_SMUL_C:
        case OP_EQ_C_PRI:
        case OP_EQ_C_ALT:
        case OP_TRACKER_PUSH_C:
        case OP_STACK:
        case OP_PUSH_S:
        case OP_CONST_PRI:
        case OP_CONST_ALT:
        case OP_LOAD_S_PRI:
        case OP_LOAD_S_ALT:
        case OP_STOR_S_PRI:
        case OP_STOR_S_ALT:
        case OP_ADDR_PRI:
        case OP_ADDR_ALT:
        case OP_MOVS:
        case OP_CVT_I64:
        case OP_INVERT_I64:
        case OP_NEG_I64:
        case OP_SMUL_I64:
        case OP_ADD_I64:
        case OP_SUB_ALT_I64:
        case OP_SHL_I64:
        case OP_SSHR_I64:
        case OP_SHR_I64:
        case OP_EQ_I64:
        case OP_NEQ_I64:
        case OP_OR_I64:
        case OP_AND_I64:
        case OP_XOR_I64:
            fprintf(stdout, " %d", cip[1]);
            break;

        case OP_CALL:
        {
            const char* name = smx_->LookupFunction(cip[1]);
            if (name)
                fprintf(stdout, " %s", name);
            else
                fprintf(stdout, " unknown_function_%x", cip[1]);
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
            auto target = smx_->DescribeCode().bytes + cip[1];
            auto diff = uint32_t(target - reinterpret_cast<const uint8_t*>(method_start));
            fprintf(stdout, " %04x ; %x", diff, cip[1]);
            break;
        }

        case OP_SYSREQ_C:
        case OP_SYSREQ_N:
        {
            uint32_t index = cip[1];
            if (op == OP_SYSREQ_N)
                fprintf(stdout, " %u", cip[2]);
            if (index < smx_->natives().length())
                fprintf(stdout, " %s", smx_->names() + smx_->natives()[index].name);
            else
                fprintf(stdout, " unknown_native_%u", index);
            break;
        }

        case OP_PUSH2_C:
        case OP_PUSH2:
        case OP_PUSH2_S:
        case OP_PUSH2_ADR:
        case OP_SDIV_ALT_I64:
            fprintf(stdout, " %d, %d", cip[1], cip[2]);
            break;

        case OP_PUSH3_C:
        case OP_PUSH3:
        case OP_PUSH3_S:
        case OP_PUSH3_ADR:
            fprintf(stdout, " %d, %d, %d", cip[1], cip[2], cip[3]);
            break;

        case OP_PUSH4_C:
        case OP_PUSH4:
        case OP_PUSH4_S:
        case OP_PUSH4_ADR:
            fprintf(stdout, " %d, %d, %d, %d", cip[1], cip[2], cip[3], cip[4]);
            break;

        case OP_PUSH5_C:
        case OP_PUSH5:
        case OP_PUSH5_S:
        case OP_PUSH5_ADR:
            fprintf(stdout, " %d, %d, %d, %d, %d", cip[1], cip[2], cip[3], cip[4], cip[5]);
            break;

        case OP_INITARRAY_PRI:
        case OP_INITARRAY_ALT:
            fprintf(stdout, " %d %d %d %d %d", cip[1], cip[2], cip[3], cip[4], cip[5]);
            break;

        default:
            break;
    }
}

template void DumpTool::DumpCodeRangeV1<true>(cell_t pcode_start, cell_t pcode_end);
template void DumpTool::DumpCodeRangeV1<false>(cell_t pcode_start, cell_t pcode_end);
