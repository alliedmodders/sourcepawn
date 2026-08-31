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
#include "heap.h"

#include <memory>
#include <algorithm>
#include <mimalloc.h>

#include "environment.h"

namespace sp {

VirtMem& GetVirtMem() {
    return Environment::get()->virt_mem();
}

Heap::Heap() {
}

Heap::~Heap() {
    while (first_) {
        Chunk* next = first_->next;
        delete first_;
        first_ = next;
    }
}

bool Heap::Initialize() {
    size_t initial_size = kDefaultStackSize + kDefaultHeapChunkSize;
    first_ = NewChunk(initial_size);
    if (!first_)
        return false;

    current_ = first_;
    return true;
}

Heap::Chunk::~Chunk() {
    if (base) {
        mi_free(base);
    }
}

Heap::Chunk* Heap::NewChunk(size_t size) {
    auto chunk = std::make_unique<Chunk>();
    
    // Allocate the large chunk using mimalloc
    chunk->base = (uint8_t*)mi_malloc(size);
    if (!chunk->base) {
        Environment::get()->ReportError(SP_ERROR_OUT_OF_MEMORY);
        return nullptr;
    }
    chunk->size = size;
    chunk->end = chunk->base + size;
    chunk->pos = chunk->base;

    return chunk.release();
}

uint8_t* Heap::SlowAllocate(uint32_t size) {
    if (current_->next && current_->next->size >= size) {
        current_ = current_->next;
        current_->pos = current_->base;
        return current_->Allocate(size);
    }

    size_t sized_up = std::max((size_t)size, (size_t)kDefaultHeapChunkSize);

    Chunk* new_chunk = NewChunk(sized_up);
    if (!new_chunk)
        return nullptr;

    if (current_->next)
        new_chunk->next = current_->next;
    current_->next = new_chunk;
    current_ = new_chunk;
    return current_->Allocate(size);
}

uint8_t* Heap::Allocate(uint32_t requested_size) {
    size_t aligned_size = ke::Align(requested_size, sizeof(uint32_t));
    if (!current_->CanAllocate(aligned_size))
        return SlowAllocate(aligned_size);
    return current_->Allocate(aligned_size);
}

Heap::Position Heap::GetPosition() {
    Position hp;
    hp.chunk = current_;
    hp.pos = current_->pos;
    return hp;
}

void Heap::RestorePosition(const Position& hp) {
    auto chunk = reinterpret_cast<Chunk*>(hp.chunk);
    assert(ValidateRestoreTo(chunk, hp.pos));
    current_ = chunk;
    current_->pos = hp.pos;
}

bool Heap::ValidateRestoreTo(Chunk* chunk, uint8_t* pos) {
    assert(chunk->Owns(pos));
    assert(pos <= chunk->pos);

    if (chunk == current_)
        return true;

    for (auto iter = first_; iter; iter = iter->next) {
        if (chunk == iter)
             return true;

        if (chunk == current_)
            return false;
    }
    return false;
}

} // namespace sp
