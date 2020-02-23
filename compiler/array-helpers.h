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
//
//  Version: $Id$
#pragma once

#include "parse-node.h"

// Determine the static size of an iARRAY based on dimension expressions and
// array initializers. The array may be converted to an iREFARRAY if it is
// determined to be dynamic.
void ResolveArraySize(VarDecl* decl);
void ResolveArraySize(const token_pos_t& pos, typeinfo_t* type, int vclass);

// Perform type and size checks of an array and its initializer if present.
bool CheckArrayDeclaration(VarDecl* decl);
bool CheckArrayInitialization(const typeinfo_t& type, Expr* init);

struct ArrayData {
    std::vector<cell> iv;
    std::vector<cell> data;
    uint32_t zeroes;

    size_t total_size() const {
        return iv.size() + data.size() + zeroes;
    }
};

void BuildArrayInitializer(VarDecl* decl, ArrayData* array, cell base_addr);
void BuildArrayInitializer(const typeinfo_t& type, Expr* init, ArrayData* array);

cell CalcArraySize(symbol* sym);
