// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) AlliedModders LLC 2021
//
//  This software is provided "as-is", without any express or implied warranty.
//  In no event will the authors be held liable for any damages arising from
//  the use of this software.
//
//  Permission is granted to anyone to use this software for any purpose,
//  including commercial applications, and to alter it and redistribute it
//  freely, subject to the following restrictions:
//
//  1.  The origin of this software must not be misrepresented; you must not
//      claim that you wrote the original software. If you use this software in
//      a product, an acknowledgment in the product documentation would be
//      appreciated but is not required.
//  2.  Altered source versions must be plainly marked as such, and must not be
//      misrepresented as being the original software.
//  3.  This notice may not be removed or altered from any source distribution.

#pragma once

#include "parse-node.h"
#include "symbols.h"

namespace sp {
namespace cc {

class Decl;

class SymbolScope final : public PoolObject
{
  public:
    SymbolScope(SymbolScope* parent, ScopeKind kind, int fnumber = -1)
      : parent_(parent),
        kind_(kind),
        symbols_(nullptr),
        fnumber_(fnumber)
    {}

    Decl* Find(Atom* atom) const {
        if (!symbols_)
            return nullptr;
        auto iter = symbols_->find(atom);
        if (iter == symbols_->end())
            return nullptr;
        return iter->second;
    }

    void Add(Decl* decl);

    // Add, but allow duplicates by linking together.
    void AddChain(Decl* decl);

    void ForEachSymbol(const std::function<void(Decl*)>& callback) {
        if (!symbols_)
            return;
        for (const auto& pair : *symbols_) {
            for (auto iter = pair.second; iter; iter = iter->next)
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
    tr::unordered_map<Atom*, Decl*>* symbols_;
    int fnumber_;
};

Decl* FindSymbol(SymbolScope* scope, Atom* name, SymbolScope** found = nullptr);
Decl* FindSymbol(SemaContext& sc, Atom* name, SymbolScope** found = nullptr);

} // namespace cc
} // namespace sp
