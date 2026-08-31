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
#include "objects.h"

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

const TypeDesc* TypeCache::CreateFunction(const TypeDesc* return_type, const std::vector<const TypeDesc*>& args, bool is_native) {
    FunctionLookupKey key(return_type, std::span<const TypeDesc* const>(args.data(), args.size()), is_native);
    auto p = cache_.findForAdd(key);
    if (p.found())
        return *p;

    const TypeDesc** args_copy = nullptr;
    if (!args.empty()) {
        args_copy = pool_.alloc<const TypeDesc*>(args.size());
        for (size_t i = 0; i < args.size(); i++)
            args_copy[i] = args[i];
    }
    std::span<const TypeDesc*> args_span(args_copy, args.size());
    TypeDesc* td = NewTypeDesc(pool_, return_type, args_span, is_native);

    cache_.add(p, td);
    return td;
}

const TypeDesc* TypeCache::GetSlice(const TypeDesc* elt) {
    TypeCacheKey key(TypeKind::ArraySlice, elt, 0);

    auto p = cache_.findForAdd(key);
    if (p.found())
        return *p;

    TypeDesc* td = NewTypeDesc(pool_, TypeKind::ArraySlice, elt);
    cache_.add(p, td);
    return td;
}

const TypeDesc* TypeCache::GetArray(const TypeDesc* elt) {
    TypeCacheKey key(TypeKind::Array, elt);

    auto p = cache_.findForAdd(key);
    if (p.found())
        return *p;

    TypeDesc* td = NewTypeDesc(pool_, TypeKind::Array, elt);
    if (elt->IsHeapItem())
        td->set_finalizer(SpArray::NestedFinalizer);

    cache_.add(p, td);
    return td;
}

const TypeDesc* TypeCache::GetFixedArray(const TypeDesc* elt, uint32_t size) {
    TypeCacheKey key(TypeKind::FixedArray, elt, size);

    auto p = cache_.findForAdd(key);
    if (p.found())
        return *p;

    TypeDesc* td = NewTypeDesc(pool_, elt, size);
    if (elt->IsHeapItem())
        td->set_finalizer(SpArray::NestedFinalizer);

    cache_.add(p, td);
    return td;
}

const TypeDesc* TypeCache::GetFlatArray(const TypeDesc* elt, uint32_t size) {
    assert(elt->kind() != TypeKind::FlatArray);
    TypeCacheKey key(TypeKind::FlatArray, elt, size);

    auto p = cache_.findForAdd(key);
    if (p.found())
        return *p;

    TypeDesc* td = NewTypeDesc(pool_, TypeKind::FlatArray, elt, size);
    cache_.add(p, td);
    return td;
}

const TypeDesc* TypeCache::GetReference(const TypeDesc* elt) {
    assert(!elt->IsReference());
    TypeCacheKey key(TypeKind::Reference, elt);

    auto p = cache_.findForAdd(key);
    if (p.found())
        return *p;

    TypeDesc* td = NewTypeDesc(pool_, TypeKind::Reference, elt);
    cache_.add(p, td);
    return td;
}

const TypeDesc* TypeCache::GetClassdef(v2::Runtime* rt, const smx_rtti_classdef* classdef, TypeKind kind) {
    TypeCacheKey key(kind, classdef);

    auto p = cache_.find(key);
    if (p.found())
        return *p;

    auto image = rt->image();
    images_.insert(image->shared_from_this());

    const smx_rtti_classdef* first = image->getClassdef(0);
    uint32_t cls_index = (uint32_t)(classdef - first);
    uint32_t stopat = image->getClassdefFieldsEnd(cls_index);
    uint32_t num_fields = stopat - classdef->first_field;

    bool is_class = (kind == TypeKind::Object);

    // Create a placeholder and cache it before processing fields, otherwise
    // recursion could wind up re-parsing type forever.
    TypeDesc* td = NewTypeDesc(pool_, classdef);
    td->kind_ = kind;
    if (is_class)
        td->set_finalizer(SpObject::NestedFinalizer);

    auto where = cache_.findForAdd(key);
    assert(!where.found());
    cache_.add(where, td);

    uint32_t* offsets = pool_.alloc<uint32_t>(num_fields);
    std::span<uint32_t> field_offsets(offsets, num_fields);

    // Enum structs are bare, so there's no header. Objects on the other hand
    // have an SpObject header.
    uint32_t header_offset = is_class ? sizeof(SpObject) : 0;

    std::vector<uint32_t> heap_item_list;
    uint32_t current_offset = 0;
    for (uint32_t i = 0; i < num_fields; i++) {
        field_offsets[i] = current_offset + header_offset;

        auto field = image->getField(classdef->first_field + i);
        auto field_td = rt->LoadTypeFromId(field->type_id);
        if (!field_td)
            return nullptr;

        if (field_td->IsHeapItem()) {
            if (!is_class) {
                rt->ReportErrorNumber(SP_ERROR_RTTI);
                return nullptr;
            }

            assert(is_class || (!field_td->IsArrayish() || field_td->IsFlatArray()));

            heap_item_list.push_back(field_offsets[i]);
        }
        current_offset += field_td->field_size();
        current_offset = ke::Align(current_offset, sizeof(cell_t));
    }

    // Allocate heap_item_offsets buffer.
    uint32_t* heap_offsets = nullptr;
    if (!heap_item_list.empty()) {
        heap_offsets = pool_.alloc<uint32_t>(heap_item_list.size());
        for (size_t i = 0; i < heap_item_list.size(); i++)
            heap_offsets[i] = heap_item_list[i];
    }

    std::span<uint32_t> heap_item_offsets(heap_offsets, heap_item_list.size());

    // Update placeholder with computed offsets.
    td->init_clsdef(current_offset, field_offsets, heap_item_offsets);
    return td;
}

} // namespace sp
