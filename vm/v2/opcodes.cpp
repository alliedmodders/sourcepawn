// vim: set ts=8 sw=4 tw=99 sts=4 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2004-2026 AlliedModders LLC
//
#include "v2/opcodes.h"

#include <inttypes.h>

#include "binary-reader.h"

using namespace sp::v2;
using namespace SourcePawn;

namespace sp::v2 {

const char* GetOpcodeName(OPCODE op) {
    static std::vector<const char*> names(OPCODES_LAST, nullptr);
#define FOR_EACH_OPCODE(op, val, text, bytes) names[OP_##op] = text;
    OPCODE_LIST_V2(FOR_EACH_OPCODE)
#undef FOR_EACH_OPCODE
    return names[op];
}

int GetSwitchOpcodeSize(const uint8_t* cip) {
    assert((OPCODE)*cip == OP_SWITCH);
    cip++;
    return (*reinterpret_cast<const cell_t*>(cip) * (sizeof(cell_t) * 2)) + 1 + sizeof(cell_t) * 2;
}

void SpewOpcode(FILE* fp, Runtime* runtime, const uint8_t* start, const uint8_t* cip) {
    fprintf(fp, "  [%05d:%04d]", (int)(cip - runtime->code().bytes), (int)(cip - start));

    if (*cip >= OPCODES_LAST) {
        fprintf(fp, " unknown-opcode\n");
        return;
    }

    BinaryReader reader(cip);

    OPCODE op = (OPCODE)reader.read<uint8_t>();
    fprintf(fp, " %s ", GetOpcodeName(op));

    switch (op) {
        case OP_LOAD_GLB:
        case OP_STOR_GLB:
            fprintf(fp, "%d", reader.read<uint16_t>());
            break;

        case OP_PUSH_C:
            fprintf(fp, "%d", reader.read<cell_t>());
            break;

        case OP_PUSH_C_I8:
            fprintf(fp, "%d", (int)reader.read<int8_t>());
            break;

        case OP_PUSH_C_I64:
            fprintf(fp, "%" PRId64, reader.read<int64_t>());
            break;

        case OP_PUSH_C_F32:
            fprintf(fp, "%f", reader.read<float>());
            break;

        case OP_ADDR_S:
        case OP_LOAD_S:
        case OP_STOR_S:
            fprintf(fp, "%d", reader.read<int16_t>());
            break;

        case OP_STOR_S_C: {
            int16_t offset = reader.read<int16_t>();
            cell_t value = reader.read<cell_t>();
            fprintf(fp, "%d, %d", offset, value);
            break;
        }

        case OP_LOAD_FN:
        case OP_CALL:
        case OP_CALLN:
        case OP_CALLVA:
        {
            uint32_t method_index = reader.read<uint32_t>();
            fprintf(fp, "%u", method_index);
            if (op == OP_CALLN || op == OP_CALLVA) {
                uint8_t nargs = reader.read<uint8_t>();
                fprintf(fp, ", %u", nargs);
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
            cell_t target_offs = reader.read<cell_t>();
            fprintf(fp, "%05d:%04d", target_offs, (int)((runtime->code().bytes + target_offs) - start));
            break;
        }

        default:
            break;
    }

    fprintf(fp, "\n");
}

} // namespace sp::v2
