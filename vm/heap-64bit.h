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

class Heap64 {
  public:
    ~Heap64();

    bool Initialize();

    uint8_t* Allocate(uint32_t requested_size) {
        size_t aligned_size = ke::Align(requested_size, sizeof(uint32_t));

        if (map_base_ + aligned_size > high_watermark_)
            return SlowAllocate(aligned_size);
        return FastAllocate(aligned_size);
    }

    uint8_t* FastAllocate(uint32_t size) {
        assert(ke::IsAligned(size, sizeof(uint32_t)));
        assert(map_base_ + size <= high_watermark_);

        uint8_t* p = pos_;
        pos_ += size;
        return p;
    }

    union Position {
        struct {
            uint32_t value1;
            uint32_t value2;
        } components;
        uint8_t* pos;
    };


    Position GetPosition() {
        static_assert(sizeof(uint64_t) == sizeof(uintptr_t));
        Position hp;
        hp.pos = pos_;
        return hp;
    }

    void RestorePosition(const Position& hp) {
        assert(hp.pos >= map_base_);
        assert(hp.pos <= pos_);
        pos_ = hp.pos;
    }

  private:
    uint8_t* SlowAllocate(uint32_t size);

  private:
    size_t map_len_ = 0;
    uint8_t* map_base_ = nullptr;
    uint8_t* map_end_ = nullptr;
    size_t page_size_ = 0;

    uint8_t* pos_ = nullptr;
    uint8_t* high_watermark_ = nullptr;
};

} // namespace sp
