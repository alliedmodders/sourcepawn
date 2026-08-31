// vim: set ts=8 sw=4 tw=99 sts=4 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2004-2026 AlliedModders LLC

#ifndef _INCLUDE_SOURCEPAWN_JIT_X86_OPCODES_H_
#define _INCLUDE_SOURCEPAWN_JIT_X86_OPCODES_H_

#include <smx/smx-v1-opcodes.h>
#include <sp_vm_types.h>
#include "legacy/plugin-runtime.h"

namespace sp::v1 {

void SpewOpcode(FILE* fp, PluginRuntime* runtime, const cell_t* start, const cell_t* cip);

// These count opcodes in # of cells, not bytes.
const char* GetOpcodeName(OPCODE op);
int GetCaseTableSize(const uint8_t* cip);

static inline int GetOpcodeSize(OPCODE op) {
    switch (op) {
#define FOR_EACH_OPCODE(op, val, text, cells) case OP_##op: return cells;
        OPCODE_LIST_V1(FOR_EACH_OPCODE)
#undef FOR_EACH_OPCODE
        default:
            assert(false);
            return 0;
    }
}

static inline const uint8_t*
NextInstruction(const uint8_t* cip) {
    OPCODE op = (OPCODE) * reinterpret_cast<const cell_t*>(cip);
    if (op == OP_CASETBL)
        return cip + GetCaseTableSize(cip) * sizeof(cell_t);
    return cip + GetOpcodeSize(op) * sizeof(cell_t);
}

} // namespace sp

#endif //_INCLUDE_SOURCEPAWN_JIT_X86_OPCODES_H_
