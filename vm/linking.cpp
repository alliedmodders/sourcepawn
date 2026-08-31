// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
//
#include <limits>

#include "linking.h"
#include "environment.h"
#include "macro-assembler.h"

using namespace sp;

LinkedCode sp::LinkCode(Environment* env, Assembler& masm, const char* name, const CodeDebugMap& mapping) {
    if (masm.outOfMemory())
        return {};

    auto size = masm.total_size();

    // This check ensures that 32-bit displacement always works internally.
    if (size > std::numeric_limits<int32_t>::max())
        return {};

    CodeChunk chunk = env->AllocateCode(size);
    if (!chunk)
        return {};

    LinkedCode code = { chunk, nullptr };
    masm.emitToExecutableMemory(&code);

    env->WriteDebugMetadata(code.entry, size, name, mapping);
    return code;
}
