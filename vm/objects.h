// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
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

    // Upvars follow as flexible array.
    // cell_t upvars[];  // num_upvars cells
    uint8_t* upvars() {
        assert(td->kind() == TypeKind::Closure);
        return reinterpret_cast<uint8_t*>(this + 1);
    }

    static inline uint32_t OffsetOfSlot(uint32_t slot) {
        return sizeof(SpFunction) + slot;
    }

    static void NestedFinalizer(HeapItem* obj);
};

} // namespace sp
