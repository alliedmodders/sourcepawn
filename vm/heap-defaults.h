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

#include <amtl/am-bits.h>
#if defined(KE_64BIT)
# include "heap-64bit.h"
#elif defined(KE_32BIT)
# include "heap-32bit.h"
#else
# error "KE_64BIT or KE_32BIT not defined"
#endif

namespace sp {

static constexpr uint32_t kDefaultStackSize = 1 * ke::kMB;
static constexpr uint32_t kDefaultHeapChunkSize = 16 * ke::kMB;

#ifdef KE_64BIT
using HeapImpl = Heap64;
#else
using HeapImpl = Heap32;
#endif

struct HeapSave final {
    HeapSave(HeapImpl& heap)
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

    HeapImpl& heap;
    HeapImpl::Position pos;
};

} // namespace sp
