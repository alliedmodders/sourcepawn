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

    if (mi_heap_)
        mi_heap_destroy(mi_heap_);
}

bool Heap::Initialize() {
    mi_heap_ = mi_heap_new();
    return mi_heap_ != nullptr;
}

static bool mi_cdecl VisitBlocksForEmpty(const mi_heap_t*, const mi_heap_area_t* area, void* block, size_t block_size, void* arg) {
    if (block) {
        *reinterpret_cast<bool*>(arg) = false;
#ifndef NDEBUG
        fprintf(stderr, "LEAKED BLOCK: %p, size %zu\n", block, block_size);
#endif
    }
    return true;
}

bool Heap::IsEmpty() const {
    if (!mi_heap_)
        return true;
    bool is_empty = true;
    mi_heap_visit_blocks(mi_heap_, false, VisitBlocksForEmpty, &is_empty);
    return is_empty;
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
