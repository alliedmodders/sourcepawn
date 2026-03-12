// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// Copyright (C) 2006-2015 AlliedModders LLC
//
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
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
