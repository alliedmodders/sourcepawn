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
#pragma once

#include <stdint.h>
#include <vector>

#include "code-allocator.h"

namespace sp {

class Assembler;
class Environment;

struct CodeDebugMapping;
using CodeDebugMap = std::vector<CodeDebugMapping>;

struct LinkedCode {
    CodeChunk chunk;
    uint8_t* entry = nullptr;

    size_t code_size() const { return chunk.bytes() - (entry - chunk.address()); }
};

LinkedCode LinkCode(Environment* env, Assembler& masm, const char* name,
                    const CodeDebugMap& mapping);

} // namespace sp
