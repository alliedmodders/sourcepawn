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

#include <stdint.h>

#include <span>

namespace sp {

struct smx_rtti_classdef;

enum class TypeKind : uint8_t {
    Void,
    Bool,
    Int32,
    Int64,
    Float32,
    Char8,
    Any,
    TopFunction,
    Array,
    FixedArray,
    FlatArray,
    ArraySlice,
    Reference,
    EnumStruct,
};

class TypeDesc final {
  public:
    explicit TypeDesc(TypeKind kind)
      : kind_(kind),
        can_global_cache_(true)
    {
        assert(kind < TypeKind::Array);
    }

    TypeDesc(TypeKind kind, const TypeDesc* elt)
      : kind_(kind),
        can_global_cache_(elt->can_global_cache())
    {
        assert(kind == TypeKind::Array || kind == TypeKind::ArraySlice ||
               kind == TypeKind::Reference);
        if (kind == TypeKind::Reference) {
            ref = elt;
        } else {
            array.elt = elt;
            array.rank = elt->IsArrayish() ? elt->array_rank() + 1 : 1;
        }
    }

    TypeDesc(TypeKind kind, const TypeDesc* elt, uint32_t array_size)
      : kind_(kind),
        can_global_cache_(elt->can_global_cache())
    {
        assert(kind == TypeKind::FixedArray || kind == TypeKind::FlatArray);
        array.elt = elt;
        array.size = array_size;
        array.rank = elt->IsArrayish() ? elt->array_rank() + 1 : 1;
    }

    TypeDesc(const TypeDesc* elt, uint32_t array_size)
      : TypeDesc(TypeKind::FixedArray, elt, array_size)
    {}

    explicit TypeDesc(const smx_rtti_classdef* classdef)
      : kind_(TypeKind::EnumStruct),
        can_global_cache_(false)
    {
        clsdef.classdef = classdef;
        clsdef.total_size = 0;
        clsdef.field_offsets = {};
    }

    TypeDesc(const smx_rtti_classdef* classdef, uint32_t total_size, std::span<uint32_t> field_offsets)
      : kind_(TypeKind::EnumStruct),
        can_global_cache_(false)
    {
        clsdef.classdef = classdef;
        clsdef.total_size = total_size;
        clsdef.field_offsets = field_offsets;
    }

    // Size needed to store a value of this type into a variable slot.
    uint32_t slot_size() const {
        assert(kind_ != TypeKind::Void);
        switch (kind_) {
            case TypeKind::Int64:
                return sizeof(int64_t);
            case TypeKind::FlatArray:
                return (array.size * array.elt->element_size() + 3) & ~3;
            case TypeKind::EnumStruct:
                return clsdef.total_size;
            default:
                return sizeof(int32_t);
        }
    }

    // Size needed to store a value of this type into an element of an array.
    uint32_t element_size() const {
        switch (kind_) {
            // plain old data
            case TypeKind::Bool:
            case TypeKind::Int32:
            case TypeKind::Float32:
            case TypeKind::Any:
                return sizeof(int32_t);

            case TypeKind::Int64:
                return sizeof(int64_t);

            case TypeKind::Char8:
                return sizeof(char);

            // Pointer types.
            case TypeKind::TopFunction:
            case TypeKind::Array:
            case TypeKind::FixedArray:
            case TypeKind::FlatArray:
                return sizeof(uint32_t);

            case TypeKind::EnumStruct:
                return clsdef.total_size;

            default:
                assert(false);
                return 0;
        }
    }

    uint32_t field_size() const { return slot_size(); }

    bool IsInt64() const { return kind_ == TypeKind::Int64; }

    TypeKind kind() const { return kind_; }
    bool can_global_cache() const { return can_global_cache_; }

    // For fixed arrays, length of arrays of this type.
    uint32_t array_size() const {
        assert(kind_ == TypeKind::FixedArray || kind_ == TypeKind::FlatArray);
        return array.size;
    }
    const TypeDesc* array_elt() const {
        assert(IsArrayish());
        return array.elt;
    }
    uint32_t array_rank() const {
        assert(IsArrayish());
        return array.rank;
    }
    bool IsArrayish() const {
        return kind_ == TypeKind::Array || kind_ == TypeKind::FixedArray ||
               kind_ == TypeKind::FlatArray || kind_ == TypeKind::ArraySlice;
    }
    bool IsNonFlatArray() const {
        return kind_ == TypeKind::Array || kind_ == TypeKind::FixedArray ||
               kind_ == TypeKind::ArraySlice;
    }
    bool IsFlatArray() const {
        return kind_ == TypeKind::FlatArray;
    }
    bool IsCompositeValue() const {
        return kind_ == TypeKind::FlatArray || kind_ == TypeKind::EnumStruct;
    }
    bool IsReference() const {
        return kind_ == TypeKind::Reference;
    }
    const TypeDesc* ref_type() const {
        assert(IsReference());
        return ref;
    }

    bool HasClassdef() const {
        return kind_ == TypeKind::EnumStruct;
    }
    const smx_rtti_classdef* cls() const {
        assert(HasClassdef());
        return clsdef.classdef;
    }
    uint32_t cls_size() const {
        assert(HasClassdef());
        return clsdef.total_size;
    }
    std::span<uint32_t> cls_offsets() const {
        assert(HasClassdef());
        return clsdef.field_offsets;
    }

  private:
    TypeKind kind_;
    bool can_global_cache_ = false;
    union {
        struct {
            const TypeDesc* elt;
            uint8_t rank;
            uint32_t size;
        } array;
        const TypeDesc* ref;
        struct {
            const smx_rtti_classdef* classdef;
            uint32_t total_size;
            std::span<uint32_t> field_offsets;
        } clsdef;
    };
};

} // namespace sp
