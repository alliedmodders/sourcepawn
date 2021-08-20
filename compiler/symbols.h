// vim: set ts=8 sts=4 sw=4 tw=99 et:
//  Pawn compiler - Recursive descend expresion parser
//
//  Copyright (c) ITB CompuPhase, 1997-2005
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
//

#pragma once

#include <functional>
#include <unordered_map>

#include "sc.h"

class SymbolScope final : public PoolObject
{
  public:
    SymbolScope(SymbolScope* parent, ScopeKind kind)
      : parent_(parent),
        kind_(kind)
    {}

    symbol* Find(sp::Atom* atom) const {
        auto iter = symbols_.find(atom);
        if (iter == symbols_.end())
            return nullptr;
        return iter->second;
    }
    void Add(symbol* sym);

    void ForEachSymbol(const std::function<void(symbol*)>& callback) {
        for (const auto& pair : symbols_) {
            symbol* iter = pair.second;
            while (iter) {
                callback(iter);
                iter = iter->next;
            }
        }
    }

    SymbolScope* parent() const { return parent_; }
    ScopeKind kind() const { return kind_; }

  private:
    SymbolScope* parent_;
    ScopeKind kind_;
    PoolMap<sp::Atom*, symbol*> symbols_;
};

class AutoEnterScope final
{
  public:
    AutoEnterScope(SymbolScope* scope);
    ~AutoEnterScope();
};

SymbolScope* CreateScope(ScopeKind kind);
SymbolScope* GetScopeChain();

symbol* findglb(sp::Atom* name);
symbol* findglb(const char* name);
symbol* findloc(sp::Atom* name, SymbolScope** scope = nullptr);
symbol* findconst(const char* name);
void delete_symbols(symbol* root, int delete_functions);
void delete_symbol(symbol* root, symbol* sym);
void markusage(symbol* sym, int usage);
symbol* addsym(const char* name, cell addr, int ident, int vclass, int tag);
symbol* addvariable(const char* name, cell addr, int ident, int vclass, int tag, int dim[],
                    int numdim, int semantic_tag);
int findnamedarg(arginfo* arg, sp::Atom* name);
symbol* find_enumstruct_field(Type* type, sp::Atom* name);
sp::Atom* operator_symname(const char* opername, int tag1, int tag2, int numtags,
                           int resulttag);
symbol* fetchfunc(const char* name);
std::string funcdisplayname(const char* funcname);
constvalue* append_constval(constvalue* table, sp::Atom* name, cell val, int index);
void delete_consttable(constvalue* table);
symbol* add_constant(const char* name, cell val, int vclass, int tag);
void reduce_referrers(symbol* root);
void deduce_liveness(symbol* root);
