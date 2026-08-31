// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

#include <assert.h>

#include <memory>
#include <unordered_set>
#include <vector>

#include <amtl/am-hashmap.h>

#include <amtl/am-hashset.h>

#include "utils/pool-allocator.h"
#include "type-desc.h"

namespace sp {

class SmxImage;

namespace v2 {
class Runtime;
} // namespace v2

struct TypeCacheKey {
    explicit TypeCacheKey(TypeKind kind)
      : kind(kind)
    {}
    TypeCacheKey(TypeKind array_kind, const TypeDesc* elt_kind)
      : kind(array_kind), elt_kind(elt_kind)
    {
        assert(kind == TypeKind::Array || kind == TypeKind::Reference);
    }
    TypeCacheKey(TypeKind array_kind, const TypeDesc* elt_kind, uint32_t size)
      : kind(array_kind), elt_kind(elt_kind), size(size)
    {
        assert(kind == TypeKind::Array || kind == TypeKind::FixedArray ||
               kind == TypeKind::FlatArray || kind == TypeKind::ArraySlice);
    }
    explicit TypeCacheKey(const smx_rtti_classdef* classdef)
      : kind(TypeKind::EnumStruct), classdef(classdef)
    {}
    TypeCacheKey(TypeKind kind, const smx_rtti_classdef* classdef)
      : kind(kind), classdef(classdef)
    {
        assert(kind == TypeKind::EnumStruct || kind == TypeKind::Object);
    }

    bool operator ==(const TypeCacheKey& other) const {
        if (kind != other.kind)
            return false;
        if (kind == TypeKind::EnumStruct || kind == TypeKind::Object)
            return classdef == other.classdef;
        if (kind == TypeKind::Array || kind == TypeKind::ArraySlice || kind == TypeKind::Reference)
            return elt_kind == other.elt_kind;
        if (kind == TypeKind::FixedArray || kind == TypeKind::FlatArray)
            return elt_kind == other.elt_kind && size == other.size;
        return true;
    }

    TypeKind kind;
    const TypeDesc* elt_kind = nullptr;
    uint32_t size = 0;
    const smx_rtti_classdef* classdef = nullptr;
};

struct FunctionLookupKey {
    FunctionLookupKey(const TypeDesc* return_type, std::span<const TypeDesc* const> args, bool is_native)
      : return_type(return_type), args(args), is_native(is_native)
    {}

    const TypeDesc* return_type;
    std::span<const TypeDesc* const> args;
    bool is_native;
};

struct ClosureLookupKey {
    ClosureLookupKey(const TypeDesc* signature, std::span<const TypeDesc* const> upvar_types)
      : signature(signature), upvar_types(upvar_types)
    {}

    const TypeDesc* signature;
    std::span<const TypeDesc* const> upvar_types;
};

class TypeCache final {
  public:
    TypeCache();

    const TypeDesc* GetPrimitive(TypeKind kind);
    const TypeDesc* GetFixedArray(const TypeDesc* elt, uint32_t size);
    const TypeDesc* GetFlatArray(const TypeDesc* elt, uint32_t size);
    const TypeDesc* GetArray(const TypeDesc* elt);
    const TypeDesc* GetSlice(const TypeDesc* elt);
    const TypeDesc* GetReference(const TypeDesc* elt);
    const TypeDesc* GetClassdef(v2::Runtime* rt, const smx_rtti_classdef* classdef, TypeKind kind);
    const TypeDesc* CreateFunction(const TypeDesc* return_type, const std::vector<const TypeDesc*>& args, bool is_native);
    const TypeDesc* GetClosure(const TypeDesc* signature, std::span<const TypeDesc* const> upvar_types);

  private:
    PoolAllocator pool_;

    struct CachePolicy {
        static bool matches(const TypeCacheKey& key, const TypeDesc* td) {
            if (td->kind() != key.kind)
                return false;
            switch (key.kind) {
                case TypeKind::EnumStruct:
                case TypeKind::Object:
                    return td->cls() == key.classdef;
                case TypeKind::Array:
                case TypeKind::ArraySlice:
                    return td->array_elt() == key.elt_kind;
                case TypeKind::FixedArray:
                case TypeKind::FlatArray:
                    return td->array_elt() == key.elt_kind && td->array_size() == key.size;
                case TypeKind::Reference:
                    return td->ref_type() == key.elt_kind;
                default:
                    return true;
            }
        }
        static bool matches(const FunctionLookupKey& key, const TypeDesc* td) {
            if (!td->IsFunction())
                return false;
            if (td->return_type() != key.return_type || td->is_native() != key.is_native)
                return false;
            if (td->args().size() != key.args.size())
                return false;
            for (size_t i = 0; i < key.args.size(); i++) {
                if (td->args()[i] != key.args[i])
                    return false;
            }
            return true;
        }
        static bool matches(const ClosureLookupKey& key, const TypeDesc* td) {
            if (!td->IsClosure())
                return false;
            if (td->closure_signature() != key.signature)
                return false;
            if (td->upvar_types().size() != key.upvar_types.size())
                return false;
            for (size_t i = 0; i < key.upvar_types.size(); i++) {
                if (td->upvar_types()[i] != key.upvar_types[i])
                    return false;
            }
            return true;
        }
        static uintptr_t hash(const TypeCacheKey& key) {
            uintptr_t h = ke::HashIntPtr((uint8_t)key.kind);
            if (key.kind == TypeKind::EnumStruct || key.kind == TypeKind::Object) {
                h = ke::HashCombine(h, ke::HashPointer(key.classdef));
            } else {
                h = ke::HashCombine(h, ke::HashPointer(key.elt_kind));
                h = ke::HashCombine(h, ke::HashIntPtr(key.size));
            }
            return h;
        }
        static uintptr_t hash(const FunctionLookupKey& key) {
            uintptr_t h = ke::HashPointer(key.return_type);
            h = ke::HashCombine(h, ke::HashIntPtr(key.is_native ? 1 : 0));
            h = ke::HashCombine(h, ke::HashIntPtr(key.args.size()));
            for (const auto& arg : key.args)
                h = ke::HashCombine(h, ke::HashPointer(arg));
            return h;
        }
        static uintptr_t hash(const ClosureLookupKey& key) {
            uintptr_t h = ke::HashPointer(key.signature);
            h = ke::HashCombine(h, ke::HashIntPtr(key.upvar_types.size()));
            for (const auto& ut : key.upvar_types)
                h = ke::HashCombine(h, ke::HashPointer(ut));
            return h;
        }
        static uintptr_t hash(const TypeDesc* td) {
            if (td->IsFunction())
                return hash(FunctionLookupKey(td->return_type(), td->args(), td->is_native()));
            if (td->IsClosure())
                return hash(ClosureLookupKey(td->closure_signature(), td->upvar_types()));
            uintptr_t h = ke::HashIntPtr((uint8_t)td->kind());
            if (td->kind() == TypeKind::EnumStruct || td->kind() == TypeKind::Object) {
                h = ke::HashCombine(h, ke::HashPointer(td->cls()));
            } else {
                const TypeDesc* elt_kind = td->IsReference() ? td->ref_type() : td->array_elt();
                uintptr_t size = (td->kind() == TypeKind::FixedArray || td->kind() == TypeKind::FlatArray) ? td->array_size() : 0;
                h = ke::HashCombine(h, ke::HashPointer(elt_kind));
                h = ke::HashCombine(h, ke::HashIntPtr(size));
            }
            return h;
        }
    };
    ke::HashSet<TypeDesc*, CachePolicy> cache_;
    std::vector<TypeDesc*> primitives_;
    std::unordered_set<std::shared_ptr<SmxImage>> images_;
};

} // namespace sp
