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
#pragma once

#include <assert.h>
#include <stdint.h>

#include "type-desc.h"

namespace sp {

struct HeapItem {
    HeapItem(const TypeDesc* td)
      : td(td),
        rc(1)
    {}

    const TypeDesc* td;
    uintptr_t rc;

    void AddRef() { rc++; }
    void Release() {
        assert(rc >= 1);
        if (--rc == 0)
            Destroy(this);
    }

  private:
    static void Destroy(HeapItem* item);
};

struct SpArray : public HeapItem {
    uint32_t length;
    uint32_t data;

    static void NestedFinalizer(HeapItem* obj);
};

struct SpObject : public HeapItem {
};

} // namespace sp
