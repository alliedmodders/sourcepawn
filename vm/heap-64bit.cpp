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
#include "heap-64bit.h"

#ifdef KE_POSIX
#include <sys/mman.h>
#include <unistd.h>
#endif

#include <algorithm>

#include <amtl/am-bits.h>
#include <amtl/am-platform.h>
#include "environment.h"
#include "heap-defaults.h"

namespace sp {

using namespace ke;

static constexpr size_t kDefaultHeapSize = 16 * kMB;

bool Heap64::Initialize() {
#ifdef KE_POSIX
# if !defined(MAP_NORESERVE)
#  define MAP_NORESERVE 0
# endif
    map_len_ = kDefaultHeapSize;
    page_size_ = getpagesize();

    constexpr int flags = MAP_ANONYMOUS | MAP_PRIVATE | MAP_NORESERVE;
    void* base = mmap(nullptr, map_len_, PROT_NONE, flags, -1, 0);
    if (base == MAP_FAILED) {
        Environment::get()->ReportError(SP_ERROR_OUT_OF_MEMORY);
        return false;
    }
    map_base_ = (uint8_t*)base;
#else
    return false;
#endif

    // Note: we skip one page, so that access of the 0th page crash.
    assert(map_len_ >= page_size_);
    pos_ = map_base_ + page_size_;
    high_watermark_ = pos_;
    map_end_ = map_base_ + map_len_;
    return true;
}

Heap64::~Heap64() {
    if (!map_base_)
        return;

#ifdef KE_POSIX
    munmap(map_base_, map_len_);
#else
    assert(false);
#endif
}

uint8_t* Heap64::SlowAllocate(uint32_t size) {
    assert(ke::IsAligned(size, sizeof(uint32_t)));

    if ((pos_ - map_base_) + size > map_len_) {
        Environment::get()->ReportError(SP_ERROR_OUT_OF_MEMORY);
        return nullptr;
    }

    size_t uncommitted = map_end_ - high_watermark_;

    // Round "needed" up to the nearest page size. This will never over allocate,
    // since the watermark is set at page boundaries.
    size_t available_now = high_watermark_ - pos_;
    size_t needed = ke::Align(size - available_now, page_size_);
    assert(needed <= uncommitted);

    assert(ke::IsAligned(kDefaultHeapChunkSize, page_size_));
    if (needed < kDefaultHeapChunkSize && uncommitted >= kDefaultHeapChunkSize)
        needed = kDefaultHeapChunkSize;

#ifdef KE_POSIX
    if (mprotect(high_watermark_, needed, PROT_READ | PROT_WRITE) == -1) {
        Environment::get()->ReportError(SP_ERROR_OUT_OF_MEMORY);
        return nullptr;
    }
#else
    assert(false);
#endif

    high_watermark_ += needed;
    return FastAllocate(size);
}

} // namespace sp
