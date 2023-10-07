// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) 2023 AlliedModders LLC
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

#include <memory>

#include "lexer.h"
#include "pool-allocator.h"

namespace sp {

using namespace cc;

class CompileContext;
class SemaContext;

struct funcenum_t : public PoolObject
{
    funcenum_t()
     : tag(0),
       name(),
       anonymous(false)
    {}
    int tag;
    Atom* name;
    PoolArray<functag_t*> entries;
    bool anonymous;
};

struct methodmap_method_t : public PoolObject
{
    explicit methodmap_method_t(methodmap_t* parent)
     : name(),
       parent(parent),
       target(nullptr),
       getter(nullptr),
       setter(nullptr),
       is_static(false)
    {}

    Atom* name;
    methodmap_t* parent;
    symbol* target;
    symbol* getter;
    symbol* setter;
    bool is_static;

    int property_tag() const;
};

struct methodmap_t : public SymbolData
{
    methodmap_t(methodmap_t* parent, Atom* name);

    methodmap_t* asMethodmap() override { return this; }

    methodmap_t* parent;
    int tag;
    bool nullable;
    bool keyword_nullable;
    Atom* name;
    PoolMap<Atom*, methodmap_method_t*> methods;

    bool must_construct_with_new() const {
        return nullable || keyword_nullable;
    }

    // Shortcut.
    methodmap_method_t* dtor;
    methodmap_method_t* ctor;

    // Set in MethodmapDecl::Bind.
    bool is_bound;

    // Original enum list.
    EnumData* enum_data;
};

/**
 * Function enumeration tags
 */
funcenum_t* funcenums_add(CompileContext& cc, Atom* name, bool anonymous);
funcenum_t* funcenum_for_symbol(CompileContext& cc, symbol* sym);
functag_t* functag_from_tag(int tag);

/**
 * Method maps.
 */
methodmap_t* methodmap_add(CompileContext& cc, methodmap_t* parent, Atom* name);
methodmap_t* methodmap_find_by_name(Atom* name);
methodmap_method_t* methodmap_find_method(methodmap_t* map, Atom* name);

} // namespace sp
