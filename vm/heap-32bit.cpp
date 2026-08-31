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

#if defined(_WIN32)
#    include <windows.h>
#    include <memoryapi.h>
#else
#    include <sys/mman.h>
#    include <unistd.h>
#    include <stdio.h>
#    include <errno.h>
#endif

#include "../utils/procmap.h"

#include "environment.h"
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

// For each platform, we need the address to be in the lower 32 bits of the
// address space, so we can tag the top bit of the pointer.
static void* AllocChunkMem(size_t size) {
#if defined(_WIN32)
    MEM_ADDRESS_REQUIREMENTS req{};
    req.HighestEndingAddress = (PVOID)0x7FFFFFFF;

    MEM_EXTENDED_PARAMETER param{};
    param.Type = MemExtendedParameterAddressRequirements;
    param.Pointer = &req;

    return VirtualAlloc2(GetCurrentProcess(), nullptr, size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE, &param, 1);
#elif defined(MAP_FIXED_NOREPLACE)
    uintptr_t search_start = 0x10000;
    while (true) {
        std::optional<uintptr_t> candidate =
            sp::FindNextMmapCandidate(search_start, size, 0x80000000);
        if (!candidate.has_value()) {
            return nullptr;
        }

        void* ptr = mmap((void*)*candidate, size, PROT_READ | PROT_WRITE,
                         MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED_NOREPLACE, -1, 0);
        if (ptr != MAP_FAILED) {
            return ptr;
        }

        if (errno != EEXIST)
            return nullptr;
        search_start = *candidate + 0x10000;
    }
#else
    // Fallback: probe-and-hint
    uintptr_t hint = 0x10000;
    while (hint + size <= 0x80000000) {
        void* ptr = mmap((void*)hint, size, PROT_READ | PROT_WRITE,
                         MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        if (ptr != MAP_FAILED) {
            if ((uintptr_t)ptr + size <= 0x80000000) {
                return ptr;
            }
            munmap(ptr, size);
        }
        hint += size;
    }
#endif
    return nullptr;
}

static void FreeChunkMem(void* base, size_t size) {
#if defined(_WIN32)
    VirtualFree(base, 0, MEM_RELEASE);
#else
    munmap(base, size);
#endif
}

Heap32::Chunk::~Chunk() {
    if (base)
        FreeChunkMem(base, size);
}

Heap32::Chunk* Heap32::NewChunk(size_t size) {
    // Use make_unique carefully since large allocations can fail
    auto chunk = std::make_unique<Chunk>();
    chunk->base = (uint8_t*)AllocChunkMem(size);
    if (!chunk->base) {
        Environment::get()->ReportError(SP_ERROR_OUT_OF_MEMORY);
        return nullptr;
    }
    chunk->size = size;
    chunk->end = chunk->base + size;
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
