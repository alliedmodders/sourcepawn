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

#include <stdio.h>

#include <algorithm>
#include <memory>
#include <string>

#include <mimalloc.h>
#include "environment.h"
#include "smx-image.h"

namespace sp {

RawHeap::RawHeap(VirtMem& virt_mem) : virt_mem_(virt_mem) {
}

RawHeap::~RawHeap() {

    if (mi_heap_)
        mi_heap_destroy(mi_heap_);
}

bool RawHeap::Initialize() {
    mi_heap_ = mi_heap_new();
    return mi_heap_ != nullptr;
}

static std::string DescribeType(const TypeDesc* td) {
    switch (td->kind()) {
        case TypeKind::Array:
        case TypeKind::ArraySlice:
            return (td->kind() == TypeKind::Array ? "array of " : "slice of ") +
                   DescribeType(td->array_elt());
        case TypeKind::FixedArray:
        case TypeKind::FlatArray:
            return DescribeType(td->array_elt()) + "[" + std::to_string(td->array_size()) + "]";
        case TypeKind::Reference:
            return "ref to " + DescribeType(td->ref_type());
        case TypeKind::Object:
            if (auto image = td->image())
                return image->names() + td->cls()->name;
            return "object";
        case TypeKind::Closure:
            return "closure";
        case TypeKind::Function:
            return "function";
        default:
            return "cell";
    }
}

struct LeakVisitInfo {
    std::string report;
};

static bool mi_cdecl VisitBlocksForLeaks(const mi_heap_t*, const mi_heap_area_t*, void* block, size_t block_size, void* arg) {
    auto* info = reinterpret_cast<LeakVisitInfo*>(arg);
    if (!block)
        return true;

    auto* item = reinterpret_cast<HeapItem*>(block);
    std::string desc = DescribeType(item->td);
    info->report += ke::StringPrintf("  %p: %s (%zu bytes)\n", block, desc.c_str(), block_size);
    return true;
}

std::string Heap::LiveObjectReport() const {
    if (!mi_heap_)
        return {};

    LeakVisitInfo info;
    mi_heap_visit_blocks(mi_heap_, true, VisitBlocksForLeaks, &info);
    return info.report;
}

void* RawHeap::AllocRaw(size_t bytes) {
    void* p = mi_heap_malloc(mi_heap_, bytes);
    if (!p) {
        Environment::get()->ReportError(SP_ERROR_OUT_OF_MEMORY);
    }
    return p;
}

void RawHeap::FreeRaw(void* ptr) {
    mi_free(ptr);
}

} // namespace sp
