// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
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
