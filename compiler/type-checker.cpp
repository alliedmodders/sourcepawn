// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) AlliedModders 2021
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
#include "type-checker.h"

#include "sc.h"
#include "symbols.h"

typeinfo_t
TypeInfoFromSymbol(symbol* sym)
{
    typeinfo_t type = {};

    type.set_tag(sym->tag);
    type.is_const = sym->is_const;

    if (sym->parent() && sym->parent()->ident == iENUMSTRUCT) {
        if (sym->dim.array.length) {
            type.ident = iARRAY;
            type.dim.emplace_back(sym->dim.array.length);
        } else {
            type.ident = iVARIABLE;
        }
        type.set_tag(sym->x.tags.index);
    } else {
        type.ident = sym->ident;

        if (sym->ident == iARRAY || sym->ident == iREFARRAY) {
            for (symbol* iter = sym; iter; iter = iter->array_child()) {
                if (iter->x.tags.index && iter->dim.array.level == 0)
                    type.declared_tag = iter->x.tags.index;
                if (iter->x.tags.index) {
                    type.declared_tag = iter->x.tags.index;
                    type.set_tag(0);
                }
                type.dim.emplace_back(iter->dim.array.length);
            }
        }
    }
    return type;
}

typeinfo_t
TypeInfoFromTag(int tag)
{
    typeinfo_t type = {};
    type.set_tag(tag);
    type.ident = iEXPRESSION;

    return type;
}
