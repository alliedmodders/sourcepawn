// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// Copyright (C) 2026 AlliedModders LLC
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

#include <assert.h>
#include <stddef.h>
#include <stdint.h>

#include <amtl/am-bits.h>

namespace sp {

class VirtMem32 {
  public:
    bool Initialize();

    uint32_t ToLocalAddr(void* p) {
        static_assert(sizeof(uint32_t) == sizeof(uintptr_t));
        return reinterpret_cast<uint32_t>(p);
    }
    template <typename T>
    T ToPhysAddr(uint32_t addr) {
        return reinterpret_cast<T>(addr);
    }
};

} // namespace sp
