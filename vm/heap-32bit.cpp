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
#include "heap-32bit.h"

#include <memory>

#include "heap-defaults.h"

namespace sp {

bool Heap32::Initialize() {
    size_t initial_size = kDefaultStackSize + kDefaultHeapChunkSize;
    first_ = NewChunk(initial_size);
    if (!first_)
        return false;

    current_ = first_;
    return true;
}

Heap32::~Heap32() {
    while (first_) {
        Chunk* next = first_->next;
        delete first_;
        first_ = next;
    }
}

Heap32::Chunk::~Chunk() {
    if (base)
        free(base);
}

Heap32::Chunk* Heap32::NewChunk(size_t size) {
    // Use malloc here since make_unique doesn't take std::nothrow, and large
    // allocations can fail.
    auto chunk = std::make_unique<Chunk>();
    chunk->base = (uint8_t*)malloc(size);
    if (!chunk->base)
        return nullptr;

    chunk->size = size;
    chunk->end = chunk->base + chunk->size;
    chunk->pos = chunk->base;

    committed_ += size;
    return chunk.release();
}

uint8_t* Heap32::SlowAllocate(uint32_t size) {
    if (current_->next && current_->next->size >= size) {
        current_ = current_->next;
        current_->pos = current_->base;
        return current_->Allocate(size);
    }

    size_t sized_up = std::max(size, kDefaultHeapChunkSize);

    Chunk* new_chunk = NewChunk(sized_up);
    if (!new_chunk)
        return nullptr;

    if (current_->next)
        new_chunk->next = current_->next;
    current_->next = new_chunk;
    current_ = new_chunk;
    return current_->Allocate(size);
}

bool Heap32::ValidateRestoreTo(Chunk* chunk, uint8_t* pos) {
    assert(chunk->Owns(pos));

    // We shouldn't be restoring to a position that was previously unused.
    assert(pos <= chunk->pos);

    if (chunk == current_)
        return true;

    for (auto iter = first_; iter; iter = iter->next) {
        if (chunk == iter)
            return true;

        // Should not restore to a position beyond the current.
        if (chunk == current_)
            return false;
    }
    return false;
}

} // namespace sp
