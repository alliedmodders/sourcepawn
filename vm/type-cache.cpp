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
#include "v2/runtime.h"
#include "smx-image.h"

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
    if (index >= primitives_.size())
        primitives_.resize(index + 1);

    if (!primitives_[index])
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
    TypeCacheKey key(TypeKind::Array, elt);

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

const TypeDesc* TypeCache::GetFlatArray(const TypeDesc* elt, uint32_t size) {
    assert(elt->kind() != TypeKind::FlatArray);
    TypeCacheKey key(TypeKind::FlatArray, elt, size);

    auto p = cache_.findForAdd(key);
    if (p.found())
        return p->value;

    TypeDesc* td = NewTypeDesc(pool_, TypeKind::FlatArray, elt, size);
    cache_.add(p, key, td);
    return td;
}

const TypeDesc* TypeCache::GetReference(const TypeDesc* elt) {
    assert(!elt->IsReference());
    TypeCacheKey key(TypeKind::Reference, elt);

    auto p = cache_.findForAdd(key);
    if (p.found())
        return p->value;

    TypeDesc* td = NewTypeDesc(pool_, TypeKind::Reference, elt);
    cache_.add(p, key, td);
    return td;
}

const TypeDesc* TypeCache::GetEnumStruct(v2::Runtime* rt, const smx_rtti_classdef* classdef) {
    TypeCacheKey key(classdef);

    auto p = cache_.find(key);
    if (p.found())
        return p->value;

    auto image = rt->image();
    images_.insert(image->shared_from_this());

    const smx_rtti_classdef* first = image->getClassdef(0);
    uint32_t cls_index = (uint32_t)(classdef - first);
    uint32_t stopat = image->getClassdefFieldsEnd(cls_index);
    uint32_t num_fields = stopat - classdef->first_field;

    uint32_t* offsets = pool_.alloc<uint32_t>(num_fields);
    std::span<uint32_t> field_offsets(offsets, num_fields);

    uint32_t current_offset = 0;
    for (uint32_t i = 0; i < num_fields; i++) {
        field_offsets[i] = current_offset;

        auto field = image->getField(classdef->first_field + i);
        auto field_td = rt->LoadTypeFromId(field->type_id);
        if (!field_td)
            return nullptr;
        assert(!field_td->IsArrayish() || field_td->IsFlatArray());
        current_offset += field_td->field_size();
        current_offset = ke::Align(current_offset, sizeof(cell_t));
    }
    uint32_t total_size = current_offset;

    TypeDesc* td = NewTypeDesc(pool_, classdef, total_size, field_offsets);
    auto p2 = cache_.findForAdd(key);
    assert(!p2.found());
    cache_.add(p2, key, td);
    return td;
}

} // namespace sp
