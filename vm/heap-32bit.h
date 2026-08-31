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

class Heap32 {
  public:
    ~Heap32();
    bool Initialize();

    struct Chunk {
        ~Chunk();

        uint8_t* base = nullptr;
        uint8_t* end = nullptr;
        size_t size = 0;
        uint8_t* pos = nullptr;
        Chunk* next = nullptr;

        bool CanAllocate(size_t bytes) { return (end - pos) >= bytes; }

        uint8_t* Allocate(size_t size) {
            assert(CanAllocate(size));

            uint8_t* p = pos;
            pos += size;
            return p;
        }

        // Inclusive for |end|, since it's a valid pos (with zero space left).
        bool Owns(uint8_t* p) { return p >= base && p <= end; }
    };

    uint8_t* Allocate(uint32_t requested_size) {
        size_t aligned_size = ke::Align(requested_size, sizeof(uint32_t));
        if (!current_->CanAllocate(aligned_size))
            return SlowAllocate(aligned_size);
        return current_->Allocate(aligned_size);
    }

    union Position {
        struct {
            uint32_t value1;
            uint32_t value2;
        } components;
        struct {
            void* chunk;
            uint8_t* pos;
        } chunk_info;
    };

    Position GetPosition() {
        static_assert(sizeof(uint32_t) == sizeof(uintptr_t));
        Position hp;
        hp.chunk_info.chunk = current_;
        hp.chunk_info.pos = current_->pos;
        return hp;
    }

    void RestorePosition(const Position& hp) {
        auto chunk = reinterpret_cast<Chunk*>(hp.chunk_info.chunk);
        assert(ValidateRestoreTo(chunk, hp.chunk_info.pos));
        current_ = chunk;
        current_->pos = hp.chunk_info.pos;
    }

  private:
    uint8_t* SlowAllocate(uint32_t size);

    Chunk* NewChunk(size_t size);
    bool ValidateRestoreTo(Chunk* chunk, uint8_t* pos);

  private:
    Chunk* first_ = nullptr;
    Chunk* current_ = nullptr;
};

} // namespace sp
