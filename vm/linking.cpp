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
