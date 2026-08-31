// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2021-2026 AlliedModders LLC

#include "scopes.h"

#include "parse-node.h"

namespace sp {
namespace cc {

void SymbolScope::Add(Decl* decl) {
    if (!symbols_) {
        auto& cc = CompileContext::get();
        symbols_ = cc.NewSymbolMap();
    }

    assert(symbols_->find(decl->name()) == symbols_->end());
    symbols_->emplace(decl->name(), decl);
}

void SymbolScope::AddChain(Decl* decl) {
    if (!symbols_) {
        auto& cc = CompileContext::get();
        symbols_ = cc.NewSymbolMap();
    }

    auto iter = symbols_->find(decl->name());
    if (iter == symbols_->end()) {
        symbols_->emplace(decl->name(), decl);
    } else {
        decl->next = iter->second;
        iter->second = decl;
    }
}

Type* SymbolScope::FindType(Atom* atom) const {
    if (!types_)
        return nullptr;
    auto iter = types_->find(atom);
    if (iter == types_->end())
        return nullptr;
    return iter->second;
}

void SymbolScope::AddType(Atom* atom, Type* type) {
    if (!types_) {
        auto& cc = CompileContext::get();
        types_ = cc.NewTypeMap();
    }

    assert(types_->find(atom) == types_->end());
    types_->emplace(atom, type);
}

void SymbolScope::AddTypeChain(Atom* atom, Type* type) {
    AddType(atom, type);
}

} // namespace cc
} // namespace sp
