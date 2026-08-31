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

#include <sp_vm_types.h>
#include "type-desc.h"

namespace sp {
namespace v2 {
class MethodInfo;
} // namespace v2

struct HeapItem {
    friend class CodeStubs;

    HeapItem(const TypeDesc* td)
      : td(td),
        rc(1)
    {
#ifndef NDEBUG
        assert(td && td->magic() == TypeDesc::kMagic);
#endif
    }

    const TypeDesc* td;
    uintptr_t rc;

    void AddRef() {
#ifndef NDEBUG
        assert(td && td->magic() == TypeDesc::kMagic);
#endif
        rc++;
    }
    void Release() {
#ifndef NDEBUG
        assert(td && td->magic() == TypeDesc::kMagic);
#endif
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
    static void NestedFinalizer(HeapItem* obj);
};

struct SpFunction : public HeapItem {
    // RefPtr here would create a cycle, so we use a raw pointer.
    sp::v2::MethodInfo* method;
};

} // namespace sp
