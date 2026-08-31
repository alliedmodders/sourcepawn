// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// Copyright (C) 2006-2026 AlliedModders LLC
//
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#include "objects.h"

#include <mimalloc.h>
#include "environment.h"
#include "heap.h"

namespace sp {

void HeapItem::Destroy(HeapItem* item) {
    assert(item->rc == 0);

    if (auto finalizer = item->td->finalizer())
        finalizer(item);
    mi_free(item);
}

void SpArray::NestedFinalizer(HeapItem* obj) {
    auto env = Environment::get();
    auto& vm = env->virt_mem();

    auto array = reinterpret_cast<SpArray*>(obj);
    if (!array->data)
        return;

    auto data = vm.ToPhysAddr<cell_t*>(array->data);
    for (uint32_t i = 0; i < array->length; i++) {
        if (!data[i])
            continue;
        auto child = vm.ToPhysAddr<HeapItem*>(data[i]);
        child->Release();
    }
}

} // namespace sp
