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
#include "virtmem-64bit.h"

#ifdef KE_POSIX
#include <sys/mman.h>
#include <unistd.h>
#elif defined(_WIN32)
#include <windows.h>
#endif

#include <algorithm>

#include <amtl/am-bits.h>
#include <amtl/am-platform.h>
#include <mimalloc.h>
#include "environment.h"
#include "virtmem-constants.h"

namespace sp {

using namespace ke;

// Just grab the entire address range. Since it's reserved and not committed,
// it only costs an mmap entry and not physical pages.
static constexpr size_t kDefaultArenaSize = 2 * ke::kGB;

bool VirtMem64::Initialize() {
    map_len_ = kDefaultArenaSize;
    size_t page_size = 0;

#ifdef KE_POSIX
# if !defined(MAP_NORESERVE)
#  define MAP_NORESERVE 0
# endif
    page_size = getpagesize();

    constexpr int flags = MAP_ANONYMOUS | MAP_PRIVATE | MAP_NORESERVE;
    void* base = mmap(nullptr, map_len_, PROT_NONE, flags, -1, 0);
    if (base == MAP_FAILED) {
        Environment::get()->ReportError(SP_ERROR_OUT_OF_MEMORY);
        return false;
    }
    map_base_ = (uint8_t*)base;
#elif defined(_WIN32)
    SYSTEM_INFO info;
    GetSystemInfo(&info);
    page_size = info.dwPageSize;

    void* base = VirtualAlloc(nullptr, map_len_, MEM_RESERVE, PAGE_NOACCESS);
    if (!base) {
        Environment::get()->ReportError(SP_ERROR_OUT_OF_MEMORY);
        return false;
    }
    map_base_ = (uint8_t*)base;
#else
    return false;
#endif

    // Skip one page to avoid 0th page access, and align up to mimalloc's slice requirement.
    size_t skip_size = ke::Align(page_size, kMimallocSliceSize);
    assert(map_len_ >= skip_size);

    map_end_ = map_base_ + map_len_;

    mi_option_set(mi_option_limit_os_alloc, 1);
    mi_option_set(mi_option_allow_large_os_pages, 0);

    size_t arena_size = map_len_ - skip_size;
    arena_size &= ~(kMimallocSliceSize - 1);
    mi_manage_os_memory_ex(map_base_ + skip_size, arena_size, false, false, false, -1, false, nullptr);
    return true;
}

} // namespace sp
