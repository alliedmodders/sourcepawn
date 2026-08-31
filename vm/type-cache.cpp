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
#include "type-cache.h"

#include <utility>

#include <amtl/am-bits.h>

namespace sp {

using namespace ke;

TypeCache::TypeCache()
  : pool_(4 * kKB)
{
    cache_.init(64);
}

template <typename... Args>
static inline TypeDesc* NewTypeDesc(PoolAllocator& pool, Args&&... args) {
    auto td = pool.alloc<TypeDesc>();
    new (td) TypeDesc(std::forward<Args>(args)...);
    return td;
}

const TypeDesc* TypeCache::GetPrimitive(TypeKind kind) {
    assert(kind < TypeKind::Array);

    size_t index = (uint8_t)kind;
    if (index < primitives_.size() && primitives_[index])
        return primitives_[index];

    primitives_.resize(index + 1);
    primitives_[index] = NewTypeDesc(pool_, kind);
    return primitives_[index];
}

const TypeDesc* TypeCache::GetSlice(const TypeDesc* elt) {
    TypeCacheKey key(TypeKind::ArraySlice, elt, 0);

    auto p = cache_.findForAdd(key);
    if (p.found())
        return p->value;

    TypeDesc* td = NewTypeDesc(pool_, TypeKind::ArraySlice, elt);
    cache_.add(p, key, td);
    return td;
}

const TypeDesc* TypeCache::GetArray(const TypeDesc* elt) {
    TypeCacheKey key(TypeKind::Array, elt, 0);

    auto p = cache_.findForAdd(key);
    if (p.found())
        return p->value;

    TypeDesc* td = NewTypeDesc(pool_, TypeKind::Array, elt);
    cache_.add(p, key, td);
    return td;
}

const TypeDesc* TypeCache::GetFixedArray(const TypeDesc* elt, uint32_t size) {
    TypeCacheKey key(TypeKind::FixedArray, elt, size);

    auto p = cache_.findForAdd(key);
    if (p.found())
        return p->value;

    TypeDesc* td = NewTypeDesc(pool_, elt, size);
    cache_.add(p, key, td);
    return td;
}

} // namespace sp
