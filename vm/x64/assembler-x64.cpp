// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
//
#include "assembler-x64.h"

#include <string.h>

#include "linking.h"

namespace sp {

size_t Assembler::data_size() const {
    return address_table_.size() * sizeof(uintptr_t);
}

void Assembler::emitToExecutableMemory(LinkedCode* out) {
    assert(!outOfMemory());

    uint8_t* cursor = out->chunk.address();
    out->entry = cursor + address_table_.size() * sizeof(uintptr_t);

    // Relocate entries in address_table_ that need relocation.
    for (const auto& index : address_table_reloc_) {
        uintptr_t offset = address_table_[index];
        assert(offset < code_size());

        uint8_t* target = out->entry + offset;
        address_table_[index] = reinterpret_cast<uintptr_t>(target);
    }

    // Emit address table.
    for (auto riter = address_table_.rbegin(); riter != address_table_.rend(); riter++) {
        *reinterpret_cast<uintptr_t*>(cursor) = *riter;
        cursor += sizeof(uintptr_t);
    }

    assert(out->entry == cursor);
    assert(out->entry + code_size() <= out->chunk.address() + out->chunk.bytes());

    memcpy(out->entry, buffer(), code_size());
}

} // namespace sp
