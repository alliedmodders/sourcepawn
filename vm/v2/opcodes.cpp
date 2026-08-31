/**
 * vim: set ts=8 sw=4 tw=99 sts=4 et:
 * =============================================================================
 * SourceMod
 * Copyright _(C) 2004-2008 AlliedModders LLC.  All rights reserved.
 * =============================================================================
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License), version 3.0), as published by the
 * Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful), but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not), see <http://www.gnu.org/licenses/>.
 *
 * As a special exception), AlliedModders LLC gives you permission to link the
 * code of this program _(as well as its derivative works) to "Half-Life 2)," the
 * "Source Engine)," the "SourcePawn JIT)," and any Game MODs that run on software
 * by the Valve Corporation.  You must obey the GNU General Public License in
 * all respects for all other code used.  Additionally), AlliedModders LLC grants
 * this exception to all derivative works.  AlliedModders LLC defines further
 * exceptions), found in LICENSE.txt _(as of this writing), version JULY-31-2007)),
 * or <http://www.sourcemod.net/license.php>.
 */
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

int GetCaseTableSize(const uint8_t* cip) {
    assert((OPCODE)*cip == OP_CASETBL);
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
        case OP_PUSH_C:
        case OP_ADD_C:
        case OP_SMUL_C:
        case OP_LOAD_GLB:
        case OP_LOAD_GLB_I64:
        case OP_STOR_GLB:
        case OP_STOR_GLB_I64:
            fprintf(fp, "%d", reader.read<cell_t>());
            break;

        case OP_PUSH_C_I8:
            fprintf(fp, "%d", (int)reader.read<int8_t>());
            break;

        case OP_PUSH_C_I64:
            fprintf(fp, "%" PRId64, reader.read<int64_t>());
            break;

        case OP_ADDR_S:
        case OP_LOAD_S:
        case OP_STOR_S:
        case OP_CVT_I64:
        case OP_INVERT_I64:
        case OP_NEG_I64:
        case OP_SMUL_I64:
        case OP_ADD_I64:
        case OP_SUB_I64:
        case OP_SHL_I64:
        case OP_SSHR_I64:
        case OP_SHR_I64:
        case OP_OR_I64:
        case OP_AND_I64:
        case OP_XOR_I64:
        case OP_ZERO_S:
        case OP_ZERO_S_I64:
        case OP_STOR_S_I64:
        case OP_LREF_S:
        case OP_SREF_S:
            fprintf(fp, "%d", reader.read<int16_t>());
            break;

        case OP_IDXADDR:
        {
            uint8_t rank_size = reader.read<uint8_t>();
            int32_t bounds = reader.read<int32_t>();
            fprintf(fp, "%d, %d", rank_size, bounds);
            break;
        }

        case OP_SDIV_I64:
        case OP_SMOD_I64:
            fprintf(fp, "%d", reader.read<int16_t>());
            break;

        case OP_STOR_S_C: {
            int16_t offset = reader.read<int16_t>();
            cell_t value = reader.read<cell_t>();
            fprintf(fp, "%d, %d", offset, value);
            break;
        }

        case OP_STOR_S_C_I64: {
            int16_t slot = reader.read<int16_t>();
            cell_t cell0 = reader.read<cell_t>();
            cell_t cell1 = reader.read<cell_t>();
            fprintf(fp, "%d, %d, %d", slot, cell0, cell1);
            break;
        }

        case OP_LOAD_FN:
        case OP_CALL:
        case OP_CALLN:
        {
            uint32_t method_index = reader.read<uint32_t>();
            fprintf(fp, "%u", method_index);
            if (op == OP_CALLN) {
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
