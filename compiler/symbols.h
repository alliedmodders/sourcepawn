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

class CompileContext;

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

    // Only valid in global scope.
    symbol* FindGlobal(sp::Atom* atom, int fnumber) const;

    void Add(symbol* sym);

    // Add, but allow duplicates by linking together.
    void AddChain(symbol* sym);

    void ForEachSymbol(const std::function<void(symbol*)>& callback) {
        for (const auto& pair : symbols_) {
            for (symbol* iter = pair.second; iter; iter = iter->next)
                callback(iter);
        }
    }

    void DeleteSymbols(const std::function<bool(symbol*)>& callback);

    SymbolScope* parent() const { return parent_; }
    ScopeKind kind() const { return kind_; }

  private:
    SymbolScope* parent_;
    ScopeKind kind_;
    PoolMap<sp::Atom*, symbol*> symbols_;
};

void AddGlobal(CompileContext& cc, symbol* sym);

symbol* FindSymbol(CompileContext& cc, SymbolScope* chain, sp::Atom* name, int fnumber,
                   SymbolScope** found = nullptr);

symbol* findglb(CompileContext& cc, sp::Atom* name, int fnumber);
symbol* findloc(SymbolScope* scope, sp::Atom* name, SymbolScope** found = nullptr);
symbol* findconst(CompileContext& cc, SymbolScope* scope, sp::Atom* name, int fnumber);
void markusage(symbol* sym, int usage);
symbol* NewVariable(sp::Atom* name, cell addr, int ident, int vclass, int tag, int dim[],
                    int numdim, int semantic_tag);
int findnamedarg(arginfo* arg, sp::Atom* name);
symbol* FindEnumStructField(Type* type, sp::Atom* name);
sp::Atom* operator_symname(const char* opername, int tag1, int tag2, int numtags,
                           int resulttag);
symbol* fetchfunc(CompileContext& cc, sp::Atom* name, int fnumber);
std::string funcdisplayname(const char* funcname);
constvalue* append_constval(constvalue* table, sp::Atom* name, cell val, int index);
void delete_consttable(constvalue* table);
symbol* add_constant(CompileContext& cc, SymbolScope* scope, sp::Atom* name, cell val,
                     int vclass, int tag, int fnumber);
void reduce_referrers(CompileContext& cc);
void deduce_liveness(CompileContext& cc);
void delete_symbols(CompileContext& cc, bool delete_functions);
