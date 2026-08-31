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

#include <assert.h>

#include <amtl/am-hashmap.h>

#include "utils/pool-allocator.h"
#include "type-desc.h"

namespace sp {

struct TypeCacheKey {
    explicit TypeCacheKey(TypeKind kind)
      : kind(kind)
    {}
    TypeCacheKey(TypeKind array_kind, const TypeDesc* elt_kind, uint32_t size)
      : kind(array_kind), elt_kind(elt_kind), size(size)
    {
        assert(kind == TypeKind::Array || kind == TypeKind::FixedArray ||
               kind == TypeKind::ArraySlice);
    }

    bool operator ==(const TypeCacheKey& other) const {
        if (kind != other.kind)
            return false;
        if (kind == TypeKind::Array || kind == TypeKind::ArraySlice)
            return elt_kind == other.elt_kind;
        if (kind == TypeKind::FixedArray) {
            return elt_kind == other.elt_kind &&
                   size == other.size;
        }
        return true;
    }

    TypeKind kind;
    const TypeDesc* elt_kind = nullptr;
    uint32_t size = 0;
};

class TypeCache final {
  public:
    TypeCache();

    const TypeDesc* GetPrimitive(TypeKind kind);
    const TypeDesc* GetFixedArray(const TypeDesc* elt, uint32_t size);
    const TypeDesc* GetArray(const TypeDesc* elt);
    const TypeDesc* GetSlice(const TypeDesc* elt);

  private:
    PoolAllocator pool_;

    struct CachePolicy {
        static bool matches(const TypeCacheKey& key, const TypeCacheKey& other) {
            return key == other;
        }
        static uintptr_t hash(const TypeCacheKey& key) {
            uintptr_t h = ke::HashInt32((uint8_t)key.kind);
            h = ke::HashCombine(h, ke::HashPointer(key.elt_kind));
            h = ke::HashCombine(h, ke::HashInt64(key.size));
            return h;
        }
    };
    ke::HashMap<TypeCacheKey, TypeDesc*, CachePolicy> cache_;
    std::vector<TypeDesc*> primitives_;
};

} // namespace sp
