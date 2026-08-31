// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

#include <stdint.h>

#include <span>

namespace sp {

class SmxImage;
struct smx_rtti_classdef;

enum class TypeKind : uint8_t {
    Void,
    Bool,
    Int32,
    Int64,
    IntPtr,
    Float32,
    Float64,
    Char8,
    Int8,
    Int16,
    Any,
    TopObject,
    Function,
    LegacyVarArgs,
    Null,
    Array,
    FixedArray,
    FlatArray,
    ArraySlice,
    Reference,
    EnumStruct,
    Object,
    Closure,
};

struct HeapItem;

class TypeDesc final {
    friend class TypeCache;

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
        clsdef.image = nullptr;
        clsdef.total_size = 0;
        clsdef.field_offsets = {};
        clsdef.heap_item_offsets = {};
    }

    TypeDesc(const smx_rtti_classdef* classdef, uint32_t total_size,
             std::span<uint32_t> field_offsets, std::span<uint32_t> heap_item_offsets)
      : kind_(heap_item_offsets.empty() ? TypeKind::EnumStruct : TypeKind::Object),
        can_global_cache_(false)
    {
        clsdef.classdef = classdef;
        clsdef.image = nullptr;
        clsdef.total_size = total_size;
        clsdef.field_offsets = field_offsets;
        clsdef.heap_item_offsets = heap_item_offsets;
    }

    TypeDesc(const TypeDesc* return_type, std::span<const TypeDesc*> args, bool is_native)
      : kind_(TypeKind::Function),
        can_global_cache_(false)
    {
        func.return_type = return_type;
        func.args = args;
        func.is_native = is_native;
    }

    TypeDesc(const TypeDesc* signature, std::span<const TypeDesc*> upvar_types,
             std::span<uint32_t> upvar_slot_offsets)
      : kind_(TypeKind::Closure),
        can_global_cache_(false)
    {
        closure.signature = signature;
        closure.upvar_types = upvar_types;
        closure.upvar_slot_offsets = upvar_slot_offsets;
        uint32_t size = 0;
        for (const TypeDesc* td : upvar_types)
            size += td->slot_size();
        closure.total_size = size;
    }

    // Size needed to store a value of this type into a variable slot.
    uint32_t slot_size() const {
        assert(kind_ != TypeKind::Void);
        switch (kind_) {
            case TypeKind::Int64:
                return sizeof(int64_t);
            case TypeKind::IntPtr:
                return sizeof(intptr_t);
            case TypeKind::Float64:
                return sizeof(double);
            case TypeKind::FlatArray:
                return (array.size * array.elt->element_size() + 3) & ~3;
            case TypeKind::EnumStruct:
                return clsdef.total_size;
            case TypeKind::Object:
                return sizeof(int32_t);
            case TypeKind::LegacyVarArgs:
                assert(false);
                return 0;
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

            case TypeKind::Float64:
                return sizeof(double);

            case TypeKind::IntPtr:
                return sizeof(intptr_t);

            case TypeKind::Char8:
                return sizeof(char);

            case TypeKind::Int8:
                return sizeof(int8_t);

            case TypeKind::Int16:
                return sizeof(int16_t);

            // Pointer types.
            case TypeKind::Function:
            case TypeKind::Array:
            case TypeKind::FixedArray:
            case TypeKind::FlatArray:
            case TypeKind::Object:
            case TypeKind::TopObject:
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
    bool IsIntPtr() const { return kind_ == TypeKind::IntPtr; }
    bool IsFloat64() const { return kind_ == TypeKind::Float64; }
    bool IsInt16() const { return kind_ == TypeKind::Int16; }
    bool IsInt8() const { return kind_ == TypeKind::Int8; }

    bool IsWideInt() const {
        return IsInt64() || (IsIntPtr() && sizeof(void*) == 8);
    }

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

    bool IsFunction() const { return kind_ == TypeKind::Function || kind_ == TypeKind::Closure; }
    bool IsClosure() const { return kind_ == TypeKind::Closure; }
    const TypeDesc* fn_signature() const {
        assert(kind_ == TypeKind::Function || kind_ == TypeKind::Closure);
        return (kind_ == TypeKind::Closure) ? closure.signature : this;
    }
    const TypeDesc* closure_signature() const {
        assert(IsClosure());
        return closure.signature;
    }
    std::span<const TypeDesc*> upvar_types() const {
        assert(IsClosure());
        return closure.upvar_types;
    }
    const TypeDesc* upvar_type(uint32_t index) const {
        assert(IsClosure());
        assert(index < closure.upvar_types.size());
        return closure.upvar_types[index];
    }
    uint32_t upvar_slot_offset(uint32_t index) const {
        assert(IsClosure());
        assert(index < closure.upvar_slot_offsets.size());
        return closure.upvar_slot_offsets[index];
    }
    uint32_t closure_upvar_size() const {
        assert(IsClosure());
        return closure.total_size;
    }
    bool IsObject() const { return kind_ == TypeKind::Object; }
    const TypeDesc* return_type() const {
        if (IsFunction())
            return func.return_type;
        return closure_signature()->return_type();
    }
    std::span<const TypeDesc*> args() const {
        if (IsFunction())
            return func.args;
        return closure.signature->args();
    }
    bool is_native() const {
        if (IsFunction())
            return func.is_native;
        return closure.signature->is_native();
    }
    bool is_variadic() const {
        if (IsFunction())
            return func.args.size() > 0 && func.args.back()->IsLegacyVarArgs();
        return closure.signature->is_variadic();
    }
    uint32_t expected_argc() const {
        if (IsFunction())
            return func.args.size() - (is_variadic() ? 1 : 0);
        return closure.signature->expected_argc();
    }

    bool IsLegacyVarArgs() const { return kind_ == TypeKind::LegacyVarArgs; }

    bool IsHeapItem() const {
        switch (kind_) {
            case TypeKind::ArraySlice:
            case TypeKind::FixedArray:
            case TypeKind::Array:
            case TypeKind::Function:
            case TypeKind::Object:
            case TypeKind::Closure:
            case TypeKind::TopObject:
                return true;
            default:
                return false;
        }
    }

    bool HasClassdef() const {
        return kind_ == TypeKind::EnumStruct || kind_ == TypeKind::Object;
    }
    const smx_rtti_classdef* cls() const {
        assert(HasClassdef());
        return clsdef.classdef;
    }
    SmxImage* image() const {
        assert(HasClassdef());
        return clsdef.image;
    }
    uint32_t cls_size() const {
        assert(HasClassdef());
        return clsdef.total_size;
    }
    std::span<uint32_t> cls_offsets() const {
        assert(HasClassdef());
        return clsdef.field_offsets;
    }
    std::span<uint32_t> heap_item_offsets() const {
        assert(kind_ == TypeKind::Object);
        return clsdef.heap_item_offsets;
    }

    typedef void (*Finalizer)(HeapItem* item);
    Finalizer finalizer() const { return finalizer_; }

    static size_t OffsetOfKind() { return offsetof(TypeDesc, kind_); }

#ifndef NDEBUG
  public:
    static constexpr uint32_t kMagic = 0x54595045;
    uint32_t magic() const { return magic_; }
#endif

  private:
    void set_finalizer(Finalizer finalizer) { finalizer_ = finalizer; }
    void init_clsdef(uint32_t total_size,
                     std::span<uint32_t> field_offsets,
                     std::span<uint32_t> heap_item_offsets) {
        clsdef.total_size = total_size;
        clsdef.field_offsets = field_offsets;
        clsdef.heap_item_offsets = heap_item_offsets;
    }

  private:
#ifndef NDEBUG
    uint32_t magic_ = kMagic;
#endif
    TypeKind kind_;
    bool can_global_cache_ = false;
    Finalizer finalizer_ = nullptr;
    union {
        struct {
            const TypeDesc* elt;
            uint8_t rank;
            uint32_t size;
        } array;
        const TypeDesc* ref;
        struct {
            const smx_rtti_classdef* classdef;
            SmxImage* image;
            uint32_t total_size;
            std::span<uint32_t> field_offsets;
            std::span<uint32_t> heap_item_offsets;
        } clsdef;
        struct {
            const TypeDesc* return_type;
            std::span<const TypeDesc*> args;
            bool is_native;
        } func;
        struct {
            const TypeDesc* signature;
            std::span<const TypeDesc*> upvar_types;
            std::span<const uint32_t> upvar_slot_offsets;
            uint32_t total_size;
        } closure;
    };
};

} // namespace sp
