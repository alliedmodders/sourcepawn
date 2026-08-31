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

namespace sp {

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
        can_global_cache_(elt->can_global_cache()),
        elt_(elt)
    {
        assert(kind == TypeKind::Array || kind == TypeKind::ArraySlice ||
               kind == TypeKind::Reference);
        if (kind != TypeKind::Reference)
            array_rank_ = elt->IsArrayish() ? elt->array_rank() + 1 : 1;
    }

    TypeDesc(TypeKind kind, const TypeDesc* elt, uint32_t array_size)
      : kind_(kind),
        can_global_cache_(elt->can_global_cache()),
        elt_(elt),
        array_size_(array_size)
    {
        assert(kind == TypeKind::FixedArray || kind == TypeKind::FlatArray);
        array_rank_ = elt->IsArrayish() ? elt->array_rank() + 1 : 1;
    }

    TypeDesc(const TypeDesc* elt, uint32_t array_size)
      : TypeDesc(TypeKind::FixedArray, elt, array_size)
    {}

    // Size needed to store a value of this type into a variable slot.
    uint32_t slot_size() const {
        assert(kind_ != TypeKind::Void);
        switch (kind_) {
            case TypeKind::Int64:
                return sizeof(int64_t);
            case TypeKind::FlatArray:
                return (array_size_ * elt_->element_size() + 3) & ~3;
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

            default:
                assert(false);
                return 0;
        }
    }

    bool IsInt64() const { return kind_ == TypeKind::Int64; }

    TypeKind kind() const { return kind_; }
    bool can_global_cache() const { return can_global_cache_; }

    // For fixed arrays, length of arrays of this type.
    uint32_t array_size() const {
        assert(kind_ == TypeKind::FixedArray || kind_ == TypeKind::FlatArray);
        return array_size_;
    }
    const TypeDesc* array_elt() const {
        assert(IsArrayish());
        return elt_;
    }
    uint32_t array_rank() const {
        assert(IsArrayish());
        return array_rank_;
    }
    bool IsArrayish() const {
        return kind_ == TypeKind::Array || kind_ == TypeKind::FixedArray ||
               kind_ == TypeKind::FlatArray || kind_ == TypeKind::ArraySlice;
    }
    bool IsFlatArray() const {
        return kind_ == TypeKind::FlatArray;
    }
    bool IsReference() const {
        return kind_ == TypeKind::Reference;
    }
    const TypeDesc* ref_type() const {
        assert(IsReference());
        return elt_;
    }

  private:
    TypeKind kind_;
    bool can_global_cache_ = false;
    uint8_t array_rank_ = 0;
    const TypeDesc* elt_ = nullptr;
    uint32_t array_size_ = 0;
};

} // namespace sp
