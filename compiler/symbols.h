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

#include "sc.h"

symbol* CreateScope();

class AutoEnterScope final
{
  public:
    AutoEnterScope(symbol* scope);
    ~AutoEnterScope();
};

symbol* GetScopeChain();

symbol* findglb(sp::Atom* name);
symbol* findglb(const char* name);
symbol* findloc(const char* name, symbol** scope = nullptr);
symbol* findloc(sp::Atom* name, symbol** scope = nullptr);
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
