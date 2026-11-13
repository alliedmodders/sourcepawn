// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2021-2026 AlliedModders LLC

#pragma once

#include <amtl/am-hashtable.h>

#include "utils/string-pool.h"

namespace sp {
namespace cc {

class Decl;
class Type;

// Scope-level name tables: open-addressed maps from interned atom to
// declaration or type. Instances are owned by CompileContext and destroyed
// with it.

struct NamedDecl
{
    Atom* name;
    Decl* decl;
};

struct SymbolTablePolicy
{
    typedef NamedDecl Payload;

    static uint32_t hash(Atom* key) {
        return ke::HashPointer(key);
    }
    static bool matches(Atom* key, const Payload& other) {
        return key == other.name;
    }
};

struct NamedType
{
    Atom* name;
    Type* type;
};

struct TypeTablePolicy
{
    typedef NamedType Payload;

    static uint32_t hash(Atom* key) {
        return ke::HashPointer(key);
    }
    static bool matches(Atom* key, const Payload& other) {
        return key == other.name;
    }
};

typedef ke::HashTable<SymbolTablePolicy> SymbolTable;
typedef ke::HashTable<TypeTablePolicy> TypeTable;

} // namespace cc
} // namespace sp
