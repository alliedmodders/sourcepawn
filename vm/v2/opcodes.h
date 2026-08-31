// vim: set ts=8 sw=4 tw=99 sts=4 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2004-2026 AlliedModders LLC

#pragma once

#include <smx/smx-v2-opcodes.h>
#include <sp_vm_types.h>
#include "v2/runtime.h"

namespace sp::v2 {

void SpewOpcode(FILE* fp, Runtime* runtime, const uint8_t* start, const uint8_t* cip);

// These count opcodes in # of bytes.
const char* GetOpcodeName(OPCODE op);
int GetSwitchOpcodeSize(const uint8_t* cip);

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
    if (op == OP_SWITCH)
        return cip + GetSwitchOpcodeSize(cip);
    return cip + GetOpcodeSize(op);
}

} // namespace sp
