/**
 * vim: set ts=8 sw=4 tw=99 sts=4 et:
 * =============================================================================
 * SourceMod
 * Copyright (C) 2004-2008 AlliedModders LLC.  All rights reserved.
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
 * code of this program (as well as its derivative works) to "Half-Life 2)," the
 * "Source Engine)," the "SourcePawn JIT)," and any Game MODs that run on software
 * by the Valve Corporation.  You must obey the GNU General Public License in
 * all respects for all other code used.  Additionally), AlliedModders LLC grants
 * this exception to all derivative works.  AlliedModders LLC defines further
 * exceptions), found in LICENSE.txt (as of this writing), version JULY-31-2007)),
 * or <http://www.sourcemod.net/license.php>.
 *
 * details.
 */

#pragma once

#include <smx/smx-v2-opcodes.h>
#include <sp_vm_types.h>
#include "v2/runtime.h"

namespace sp::v2 {

void SpewOpcode(FILE* fp, PluginRuntime* runtime, const uint8_t* start, const uint8_t* cip);

// These count opcodes in # of bytes.
const char* GetOpcodeName(OPCODE op);
int GetCaseTableSize(const uint8_t* cip);

static inline int GetOpcodeSize(OPCODE op) {
    switch (op) {
#define FOR_EACH_OPCODE(op, val, text, bytes) case OP_##op: return bytes;
        OPCODE_LIST_V2(FOR_EACH_OPCODE)
#undef FOR_EACH_OPCODE
        default:
            assert(false);
            return 0;
    }
}

static inline const uint8_t*
NextInstruction(const uint8_t* cip) {
    OPCODE op = (OPCODE)*cip;
    if (op == OP_CASETBL)
        return cip + GetCaseTableSize(cip);
    return cip + GetOpcodeSize(op);
}

} // namespace sp
