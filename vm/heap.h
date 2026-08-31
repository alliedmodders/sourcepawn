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

#include "heap-defaults.h"

namespace sp {

VirtMem& GetVirtMem();

class Heap {
  public:
    Heap();
    ~Heap();

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

    template <typename T>
    T* AllocTyped() {
        return reinterpret_cast<T*>(Allocate(sizeof(T)));
    }

    uint8_t* Allocate(uint32_t requested_size);

    uint32_t ToLocalAddr(void* p) { return GetVirtMem().ToLocalAddr(p); }
    template <typename T>
    T ToPhysAddr(uint32_t addr) { return GetVirtMem().ToPhysAddr<T>(addr); }

    struct Position {
        Position() {
            chunk = nullptr;
            pos = nullptr;
        }

        void* chunk;
        uint8_t* pos;

        bool operator ==(const Position& other) const {
            return chunk == other.chunk && pos == other.pos;
        }
        bool operator !=(const Position& other) const {
            return !(*this == other);
        }
    };

    Position GetPosition();
    void RestorePosition(const Position& hp);

  private:
    uint8_t* SlowAllocate(uint32_t size);

    Chunk* NewChunk(size_t size);
    bool ValidateRestoreTo(Chunk* chunk, uint8_t* pos);

  private:
    Chunk* first_ = nullptr;
    Chunk* current_ = nullptr;
};

struct HeapSave final {
    HeapSave(Heap& heap)
      : heap(heap),
        pos(heap.GetPosition())
    {}
    HeapSave(HeapSave&& other) = default;
    HeapSave(const HeapSave& other) = delete;

    ~HeapSave() {
        heap.RestorePosition(pos);
    }

    HeapSave& operator =(HeapSave&& other) = delete;
    HeapSave& operator =(const HeapSave& other) = delete;

    Heap& heap;
    Heap::Position pos;
};

} // namespace sp
