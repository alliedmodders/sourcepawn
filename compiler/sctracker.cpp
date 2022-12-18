/* vim: set ts=8 sts=4 sw=4 tw=99 et: */
#include "sctracker.h"

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include <utility>

#include <amtl/am-raii.h>
#include <amtl/am-vector.h>
#include "compile-context.h"
#include "lexer.h"
#include "sc.h"
#include "semantics.h"
#include "symbols.h"
#include "types.h"

std::vector<std::unique_ptr<funcenum_t>> sFuncEnums;
std::vector<methodmap_t*> sMethodmaps;

std::vector<pstruct_t*> sStructs;

pstruct_t::pstruct_t(sp::Atom* name)
  : name(name)
{
}

const structarg_t*
pstructs_getarg(const pstruct_t* pstruct, sp::Atom* name)
{
    for (const auto& arg : pstruct->args) {
        if (arg->name == name)
            return arg;
    }
    return nullptr;
}

pstruct_t*
pstructs_add(sp::Atom* name)
{
    auto p = new pstruct_t(name);
    sStructs.push_back(p);
    return sStructs.back();
}

void
pstructs_free()
{
    sStructs.clear();
}

pstruct_t*
pstructs_find(sp::Atom* name)
{
    for (const auto& p : sStructs) {
        if (p->name == name)
            return p;
    }
    return nullptr;
}

void
funcenums_free()
{
    sFuncEnums.clear();
}

funcenum_t*
funcenums_add(CompileContext& cc, sp::Atom* name)
{
    auto e = std::make_unique<funcenum_t>();

    e->name = name;
    e->tag = cc.types()->defineFunction(name->chars(), e.get())->tagid();

    sFuncEnums.push_back(std::move(e));
    return sFuncEnums.back().get();
}

funcenum_t*
funcenum_for_symbol(CompileContext& cc, symbol* sym)
{
    functag_t* ft = new functag_t;
    ft->ret_tag = sym->tag;

    std::vector<funcarg_t> args;
    for (arginfo& arg : sym->function()->args) {
        funcarg_t dest;
        dest.type = arg.type;

        if (dest.type.ident != iARRAY && dest.type.ident != iREFARRAY)
          assert(dest.type.dim.empty());

        args.emplace_back(dest);
    }
    new (&ft->args) PoolArray<funcarg_t>(args);

    auto name = ke::StringPrintf("::ft:%s:%d:%d", sym->name(), sym->addr(), sym->codeaddr);
    funcenum_t* fe = funcenums_add(cc, gAtoms.add(name));
    new (&fe->entries) PoolArray<functag_t*>({ft});

    return fe;
}

// Finds a functag that was created intrinsically.
functag_t*
functag_from_tag(int tag)
{
    Type* type = CompileContext::get().types()->find(tag);
    funcenum_t* fe = type->asFunction();
    if (!fe)
        return nullptr;
    if (fe->entries.empty())
        return nullptr;
    return fe->entries.back();
}

methodmap_t::methodmap_t(methodmap_t* parent, LayoutSpec spec, sp::Atom* name)
 : parent(parent),
   tag(0),
   nullable(false),
   keyword_nullable(false),
   spec(spec),
   name(name),
   dtor(nullptr),
   ctor(nullptr),
   is_bound(false),
   enum_data(nullptr)
{
}

int
methodmap_method_t::property_tag() const
{
    auto types = CompileContext::get().types();

    assert(getter || setter);
    if (getter)
        return getter->tag;
    if (setter->function()->args.size() != 2)
        return types->tag_void();
    arginfo* valp = &setter->function()->args[1];
    if (valp->type.ident != iVARIABLE)
        return types->tag_void();
    return valp->type.tag();
}

methodmap_t*
methodmap_add(CompileContext& cc, methodmap_t* parent, LayoutSpec spec, sp::Atom* name)
{
    auto map = new methodmap_t(parent, spec, name);

    if (spec == Layout_MethodMap && parent) {
        if (parent->nullable)
            map->nullable = parent->nullable;
        if (parent->keyword_nullable)
            map->keyword_nullable = parent->keyword_nullable;
    }

    if (spec == Layout_MethodMap)
        map->tag = cc.types()->defineMethodmap(name->chars(), map)->tagid();
    else
        map->tag = cc.types()->defineObject(name->chars())->tagid();
    sMethodmaps.push_back(std::move(map));

    return sMethodmaps.back();
}

methodmap_t*
methodmap_find_by_tag(int tag)
{
    return CompileContext::get().types()->find(tag)->asMethodmap();
}

methodmap_t*
methodmap_find_by_name(sp::Atom* name)
{
    auto type = CompileContext::get().types()->find(name);
    if (!type)
        return NULL;
    return methodmap_find_by_tag(type->tagid());
}

methodmap_method_t*
methodmap_find_method(methodmap_t* map, sp::Atom* name)
{
    auto iter = map->methods.find(name);
    if (iter != map->methods.end())
        return iter->second;

    if (map->parent)
        return methodmap_find_method(map->parent, name);
    return nullptr;
}

void
methodmaps_free()
{
    sMethodmaps.clear();
}

LayoutSpec
deduce_layout_spec_by_tag(SemaContext& sc, int tag)
{
    if (methodmap_t* map = methodmap_find_by_tag(tag))
        return map->spec;

    Type* type = sc.cc().types()->find(tag);
    if (type && type->isFunction())
        return Layout_FuncTag;

    if (type && type->isStruct())
        return Layout_PawnStruct;

    if (Type* type = sc.cc().types()->find(tag)) {
      if (FindSymbol(sc.scope(), type->nameAtom()))
          return Layout_Enum;
    }

    return Layout_None;
}

LayoutSpec
deduce_layout_spec_by_name(SemaContext& sc, sp::Atom* name)
{
    Type* type = sc.cc().types()->find(name);
    if (!type)
        return Layout_None;

    return deduce_layout_spec_by_tag(sc, type->tagid());
}

const char*
layout_spec_name(LayoutSpec spec)
{
    switch (spec) {
        case Layout_None:
            return "<none>";
        case Layout_Enum:
            return "enum";
        case Layout_FuncTag:
            return "functag";
        case Layout_PawnStruct:
            return "deprecated-struct";
        case Layout_MethodMap:
            return "methodmap";
        case Layout_Class:
            return "class";
    }
    return "<unknown>";
}

bool
can_redef_layout_spec(LayoutSpec def1, LayoutSpec def2)
{
    // Normalize the ordering, since these checks are symmetrical.
    if (def1 > def2) {
        LayoutSpec temp = def2;
        def2 = def1;
        def1 = temp;
    }

    switch (def1) {
        case Layout_None:
            return true;
        case Layout_Enum:
            if (def2 == Layout_Enum || def2 == Layout_FuncTag)
                return true;
            return def2 == Layout_MethodMap;
        case Layout_FuncTag:
            return def2 == Layout_Enum || def2 == Layout_FuncTag;
        case Layout_PawnStruct:
        case Layout_MethodMap:
            return false;
        case Layout_Class:
            return false;
    }
    return false;
}
