// vim: set sts=2 ts=8 sw=2 tw=99 et:
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

namespace sp {

void
Assembler::emitToExecutableMemory(void* code)
{
  assert(!outOfMemory());

  uint8_t* base = reinterpret_cast<uint8_t*>(code);
  memcpy(base, buffer(), length());

  for (size_t i = 0; i < absolute_code_refs_.size(); i++) {
    size_t offset = absolute_code_refs_[i];
    size_t target = *reinterpret_cast<uint64_t*>(base + offset - 8);
    assert(target <= length());

    *reinterpret_cast<void**>(base + offset - 8) = base + target;
  }
}

} // namespace sp
