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

class VirtMem64 {
  public:
    bool Initialize();

    uint32_t ToLocalAddr(void* p) {
        assert((uint8_t*)p >= map_base_ && (uint8_t*)p <= map_end_);
        return static_cast<uint32_t>((uint8_t*)p - map_base_);
    }

    template <typename T>
    T ToPhysAddr(uint32_t addr) {
        if (!addr)
            return nullptr;
        assert(addr <= uint32_t(map_len_));
        return reinterpret_cast<T>(map_base_ + addr);
    }

  private:
    uint8_t* map_base_ = nullptr;
    uint8_t* map_end_ = nullptr;
    size_t map_len_ = 0;
};

} // namespace sp
