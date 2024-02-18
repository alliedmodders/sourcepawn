// vim: set ts=8 sts=4 sw=4 tw=99 et:
//  Pawn compiler - Recursive descend expresion parser
//
//  Copyright (c) ITB CompuPhase, 1997-2005
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

#include "array-data.h"
#include "parse-node.h"

namespace sp {
namespace cc {

class Semantics;
struct typeinfo_t;

// Determine the static size of an iARRAY based on dimension expressions and
// array initializers. The array may be converted to an iREFARRAY if it is
// determined to be dynamic.
bool ResolveArrayType(Semantics* sema, VarDeclBase* decl);
bool ResolveArrayType(Semantics* sema, const token_pos_t& pos, typeinfo_t* type, int vclass);

// Perform type and size checks of an array and its initializer if present.
bool CheckArrayInitialization(Semantics* sema, const typeinfo_t& type, Expr* init);

void BuildCompoundInitializer(VarDeclBase* decl, ArrayData* array, cell_t base_addr);
void BuildCompoundInitializer(Type* type, Expr* init, ArrayData* array);

cell_t CalcArraySize(Type* type);

} // namespace cc
} // namespace sp
