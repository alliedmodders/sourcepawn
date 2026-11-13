// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2021-2026 AlliedModders LLC

#include "scopes.h"

#include "compile-context.h"
#include "parse-node.h"

namespace sp {
namespace cc {

void SymbolScope::Add(Decl* decl) {
    if (!symbols_)
        symbols_ = CompileContext::get().NewSymbolTable();

    SymbolTable::Insert p = symbols_->findForAdd(decl->name());
    assert(!p.found());
    if (p.found())
        return;
    symbols_->add(p, NamedDecl{decl->name(), decl});
}

void SymbolScope::AddChain(Decl* decl) {
    if (!symbols_)
        symbols_ = CompileContext::get().NewSymbolTable();

    SymbolTable::Insert p = symbols_->findForAdd(decl->name());
    if (!p.found()) {
        symbols_->add(p, NamedDecl{decl->name(), decl});
    } else {
        decl->next = p->decl;
        p->decl = decl;
    }
}

Type* SymbolScope::FindType(Atom* atom) const {
    if (!types_)
        return nullptr;
    TypeTable::Result r = types_->find(atom);
    if (!r.found())
        return nullptr;
    return r->type;
}

void SymbolScope::AddType(Atom* atom, Type* type) {
    if (!types_)
        types_ = CompileContext::get().NewTypeTable();

    TypeTable::Insert p = types_->findForAdd(atom);
    assert(!p.found());
    if (p.found())
        return;
    types_->add(p, NamedType{atom, type});
}

void SymbolScope::AddTypeChain(Atom* atom, Type* type) {
    AddType(atom, type);
}

} // namespace cc
} // namespace sp
