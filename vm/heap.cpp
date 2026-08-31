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

Heap::Heap(VirtMem& virt_mem) : virt_mem_(virt_mem) {
}

Heap::~Heap() {
    while (first_) {
        Chunk* next = first_->next;
        delete first_;
        first_ = next;
    }
    if (mi_heap_)
        mi_heap_destroy(mi_heap_);
}

bool Heap::Initialize() {
    mi_heap_ = mi_heap_new();
    return mi_heap_ != nullptr;
}

Heap::Chunk::~Chunk() {
    if (base)
        mi_free(base);
}

Heap::Chunk* Heap::NewChunk(size_t size) {
    auto chunk = std::make_unique<Chunk>();
    
    // Allocate the large chunk using mimalloc
    chunk->base = (uint8_t*)mi_heap_malloc(mi_heap_, size);
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
    if (!current_) {
        size_t sized_up = std::max((size_t)size, (size_t)kDefaultHeapChunkSize);
        first_ = NewChunk(sized_up);
        if (!first_)
            return nullptr;
        current_ = first_;
        return current_->Allocate(size);
    }

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
    if (!current_ || !current_->CanAllocate(aligned_size))
        return SlowAllocate(aligned_size);
    return current_->Allocate(aligned_size);
}

void* Heap::AllocRaw(size_t bytes) {
    void* p = mi_heap_malloc(mi_heap_, bytes);
    if (!p) {
        Environment::get()->ReportError(SP_ERROR_OUT_OF_MEMORY);
    }
    return p;
}

void Heap::FreeRaw(void* ptr) {
    mi_free(ptr);
}

} // namespace sp
