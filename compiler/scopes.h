// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2021-2026 AlliedModders LLC

#pragma once

#include <functional>

#include "parse-node.h"
#include "symbol-table.h"
#include "symbols.h"

namespace sp {
namespace cc {

class Decl;
class Type;

class SymbolScope final : public PoolObject
{
  public:
    SymbolScope(SymbolScope* parent, ScopeKind kind, int fnumber = -1)
      : parent_(parent),
        kind_(kind),
        symbols_(nullptr),
        types_(nullptr),
        fnumber_(fnumber)
    {}

    Decl* Find(Atom* atom) const {
        if (!symbols_)
            return nullptr;
        SymbolTable::Result r = symbols_->find(atom);
        if (!r.found())
            return nullptr;
        return r->decl;
    }

    void Add(Decl* decl);

    // Add, but allow duplicates by linking together.
    void AddChain(Decl* decl);

    Type* FindType(Atom* atom) const;
    void AddType(Atom* atom, Type* type);
    void AddTypeChain(Atom* atom, Type* type);

    void ForEachSymbol(const std::function<void(Decl*)>& callback) {
        if (!symbols_)
            return;
        for (SymbolTable::iterator i(symbols_); !i.empty(); i.next()) {
            for (auto iter = i->decl; iter; iter = iter->next)
                callback(iter);
        }
    }

    bool IsGlobalOrFileStatic() const {
        return kind_ == sGLOBAL || kind_ == sFILE_STATIC;
    }
    bool IsLocalOrArgument() const {
        return kind_ == sLOCAL || kind_ == sARGUMENT;
    }

    SymbolScope* parent() const { return parent_; }
    void set_parent(SymbolScope* scope) { parent_ = scope; }

    ScopeKind kind() const { return kind_; }
    int fnumber() const { return fnumber_; }

  private:
    SymbolScope* parent_;
    ScopeKind kind_;
    SymbolTable* symbols_;
    TypeTable* types_;
    int fnumber_;
};

Decl* FindSymbol(SymbolScope* scope, Atom* name, SymbolScope** found = nullptr);
Decl* FindSymbol(SemaContext& sc, Atom* name, SymbolScope** found = nullptr);
void AddScopedType(SemaContext& sc, Type* type);

} // namespace cc
} // namespace sp
