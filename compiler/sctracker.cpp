/* vim: set ts=8 sts=4 sw=4 tw=99 et: */
#include "sctracker.h"

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include <utility>

#include <amtl/am-raii.h>
#include <amtl/am-vector.h>
#include "emitter.h"
#include "lexer.h"
#include "sc.h"
#include "symbols.h"
#include "types.h"

struct MemoryUse {
    MemoryUse(int type, int size)
     : type(type),
       size(size)
    {}
    int type; /* MEMUSE_STATIC or MEMUSE_DYNAMIC */
    int size; /* size of array for static (0 for dynamic) */
};

struct MemoryScope {
    MemoryScope(MemoryScope&& other)
     : scope_id(other.scope_id),
       kind(other.kind),
       usage(std::move(other.usage)),
       blacklisted(other.blacklisted)
    {}
    explicit MemoryScope(int scope_id, AllocScopeKind kind)
     : scope_id(scope_id),
       kind(kind),
       blacklisted(false)
    {}
    MemoryScope(const MemoryScope& other) = delete;

    MemoryScope& operator =(const MemoryScope& other) = delete;
    MemoryScope& operator =(MemoryScope&& other) {
        scope_id = other.scope_id;
        kind = other.kind;
        usage = std::move(other.usage);
        blacklisted = other.blacklisted;
        return *this;
    }

    int scope_id;
    AllocScopeKind kind;
    std::vector<MemoryUse> usage;
    bool blacklisted;
};

std::vector<MemoryScope> sStackScopes;
std::vector<MemoryScope> sHeapScopes;
std::vector<std::unique_ptr<funcenum_t>> sFuncEnums;
std::vector<std::unique_ptr<pstruct_t>> sStructs;
std::vector<std::unique_ptr<methodmap_t>> sMethodmaps;

pstruct_t::pstruct_t(sp::Atom* name)
{
    this->name = name;
}

const structarg_t*
pstructs_getarg(const pstruct_t* pstruct, sp::Atom* name)
{
    for (const auto& arg : pstruct->args) {
        if (arg->name == name)
            return arg.get();
    }
    return nullptr;
}

pstruct_t*
pstructs_add(sp::Atom* name)
{
    auto p = std::make_unique<pstruct_t>(name);
    sStructs.push_back(std::move(p));
    return sStructs.back().get();
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
            return p.get();
    }
    return nullptr;
}

structarg_t*
pstructs_addarg(pstruct_t* pstruct, const structarg_t* arg)
{
    if (pstructs_getarg(pstruct, arg->name))
        return nullptr;

    auto newarg = std::make_unique<structarg_t>();
    memcpy(newarg.get(), arg, sizeof(structarg_t));
    newarg->offs = pstruct->args.size() * sizeof(cell);
    newarg->index = pstruct->args.size();
    pstruct->args.push_back(std::move(newarg));

    return pstruct->args.back().get();
}

void
funcenums_free()
{
    sFuncEnums.clear();
}

funcenum_t*
funcenums_add(sp::Atom* name)
{
    auto e = std::make_unique<funcenum_t>();

    e->name = name;
    e->tag = gTypes.defineFunction(name->chars(), e.get())->tagid();

    sFuncEnums.push_back(std::move(e));
    return sFuncEnums.back().get();
}

funcenum_t*
funcenum_for_symbol(symbol* sym)
{
    functag_t* ft = new functag_t;

    ft->ret_tag = sym->tag;
    for (arginfo& arg : sym->function()->args) {
        if (!arg.ident)
            break;

        funcarg_t dest;
        dest.tag = arg.tag;
        dest.dims = arg.dim;
        dest.ident = arg.ident;
        dest.fconst = arg.is_const;

        if (dest.ident != iARRAY && dest.ident != iREFARRAY)
          assert(dest.dims.empty());

        ft->args.push_back(dest);
    }

    auto name = ke::StringPrintf("::ft:%s:%d:%d", sym->name(), sym->addr(), sym->codeaddr);
    funcenum_t* fe = funcenums_add(gAtoms.add(name));
    functags_add(fe, ft);

    return fe;
}

// Finds a functag that was created intrinsically.
functag_t*
functag_from_tag(int tag)
{
    Type* type = gTypes.find(tag);
    funcenum_t* fe = type->asFunction();
    if (!fe)
        return nullptr;
    if (fe->entries.empty())
        return nullptr;
    return fe->entries.back();
}

void
functags_add(funcenum_t* en, functag_t* src)
{
    en->entries.push_back(src);
}

static void
EnterMemoryScope(std::vector<MemoryScope>& frame, AllocScopeKind kind)
{
    if (frame.empty())
        frame.push_back(MemoryScope{0, kind});
    else
        frame.push_back(MemoryScope{frame.back().scope_id + 1, kind});
}

static void
AllocInScope(MemoryScope& scope, int type, int size)
{
    if (type == MEMUSE_STATIC && !scope.usage.empty() && scope.usage.back().type == MEMUSE_STATIC) {
        scope.usage.back().size += size;
    } else {
        scope.usage.push_back(MemoryUse{type, size});
    }

    pc_current_memory += size;
    pc_max_memory = std::max(pc_current_memory, pc_max_memory);
}

static int
PopScope(std::vector<MemoryScope>& scope_list)
{
    MemoryScope scope = ke::PopBack(&scope_list);
    int total_use = 0;
    while (!scope.usage.empty()) {
        assert(scope.usage.back().size <= pc_current_memory);
        total_use += scope.usage.back().size;
        scope.usage.pop_back();
    }
    pc_current_memory -= total_use;
    return total_use;
}

void
pushheaplist(AllocScopeKind kind)
{
    EnterMemoryScope(sHeapScopes, kind);
}

// Sums up array usage in the current heap tracer and convert it into a dynamic array.
// This is used for the ternary operator, which needs to convert its array usage into
// something dynamically managed.
// !Note:
// This might break if expressions can ever return dynamic arrays.
// Thus, we assert() if something is non-static here.
// Right now, this poses no problem because this type of expression is impossible:
//   (a() ? return_array() : return_array()) ? return_array() : return_array()
cell_t
pop_static_heaplist()
{
    cell_t total = 0;
    for (const auto& use : sHeapScopes.back().usage) {
        assert(use.type == MEMUSE_STATIC);
        total += use.size;
    }
    PopScope(sHeapScopes);
    return total;
}

int
markheap(int type, int size, AllocScopeKind kind)
{
    MemoryScope* scope = nullptr;
    if (kind == AllocScopeKind::Temp) {
        // Every expression should have an immediate temporary scope.
        scope = &sHeapScopes.back();
        assert(scope->kind == AllocScopeKind::Temp);
        assert(!scope->blacklisted);
    } else {
        // Declarations will have an immediate temporary scope as well, but we
        // must allocate into the normal scope before using the temporary one
        // (because it's a LIFO allocation).
        for (auto iter = sHeapScopes.rbegin(); iter != sHeapScopes.rend(); iter++) {
            if (iter->kind == AllocScopeKind::Temp) {
                assert(iter->usage.empty());
                iter->blacklisted = true;
                continue;
            }
            if (iter->kind == AllocScopeKind::Normal) {
                scope = &*iter;
                break;
            }
        }
        assert(scope);
    }

    AllocInScope(*scope, type, size);
    return size;
}

void
pushstacklist()
{
    EnterMemoryScope(sStackScopes, AllocScopeKind::Normal);
}

int
markstack(int type, int size)
{
    pc_current_stack += size;
    AllocInScope(sStackScopes.back(), type, size);
    return size;
}

// Generates code to free all heap allocations on a tracker
static void
modheap_for_scope(const MemoryScope& scope)
{
    for (size_t i = scope.usage.size() - 1; i < scope.usage.size(); i--) {
        const MemoryUse& use = scope.usage[i];
        if (use.type == MEMUSE_STATIC) {
            modheap((-1) * use.size * sizeof(cell));
        } else {
            modheap_i();
        }
    }
}

void
modstk_for_scope(const MemoryScope& scope)
{
    cell_t total = 0;
    for (const auto& use : scope.usage) {
        assert(use.type == MEMUSE_STATIC);
        total += use.size;
    }
    modstk(total * sizeof(cell));
}

void
popheaplist(bool codegen)
{
    assert(!sHeapScopes.empty());
    if (codegen)
        modheap_for_scope(sHeapScopes.back());
    PopScope(sHeapScopes);
}

void
genstackfree(int stop_id)
{
    for (size_t i = sStackScopes.size() - 1; i < sStackScopes.size(); i--) {
        const MemoryScope& scope = sStackScopes[i];
        if (scope.scope_id <= stop_id)
            break;
        modstk_for_scope(scope);
    }
}

void
genheapfree(int stop_id)
{
    for (size_t i = sHeapScopes.size() - 1; i < sHeapScopes.size(); i--) {
        const MemoryScope& scope = sHeapScopes[i];
        if (scope.scope_id <= stop_id)
            break;
        modheap_for_scope(scope);
    }
}

void
popstacklist(bool codegen)
{
    if (codegen)
        modstk_for_scope(sStackScopes.back());
    pc_current_stack -= PopScope(sStackScopes);
}

void
resetstacklist()
{
    sStackScopes.clear();
}

void
resetheaplist()
{
    sHeapScopes.clear();
}

int
stack_scope_id()
{
    return sStackScopes.back().scope_id;
}

int
heap_scope_id()
{
    return sHeapScopes.back().scope_id;
}

bool
has_stack_or_heap_scopes()
{
    return !sStackScopes.empty() || !sHeapScopes.empty();
}

methodmap_t::methodmap_t(methodmap_t* parent, LayoutSpec spec, sp::Atom* name)
 : parent(parent),
   tag(0),
   nullable(false),
   keyword_nullable(false),
   spec(spec),
   name(name),
   dtor(nullptr),
   ctor(nullptr)
{
}

methodmap_t*
methodmap_add(methodmap_t* parent, LayoutSpec spec, sp::Atom* name)
{
    auto map = std::make_unique<methodmap_t>(parent, spec, name);

    if (spec == Layout_MethodMap && parent) {
        if (parent->nullable)
            map->nullable = parent->nullable;
        if (parent->keyword_nullable)
            map->keyword_nullable = parent->keyword_nullable;
    }

    if (spec == Layout_MethodMap)
        map->tag = gTypes.defineMethodmap(name->chars(), map.get())->tagid();
    else
        map->tag = gTypes.defineObject(name->chars())->tagid();
    sMethodmaps.push_back(std::move(map));

    return sMethodmaps.back().get();
}

methodmap_t*
methodmap_find_by_tag(int tag)
{
    return gTypes.find(tag)->asMethodmap();
}

methodmap_t*
methodmap_find_by_name(sp::Atom* name)
{
    int tag = pc_findtag(name->chars());
    if (tag == -1)
        return NULL;
    return methodmap_find_by_tag(tag);
}

methodmap_method_t*
methodmap_find_method(methodmap_t* map, sp::Atom* name)
{
    for (const auto& method : map->methods) {
        if (method->name == name)
            return method.get();
    }
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
deduce_layout_spec_by_tag(int tag)
{
    if (methodmap_t* map = methodmap_find_by_tag(tag))
        return map->spec;

    Type* type = gTypes.find(tag);
    if (type && type->isFunction())
        return Layout_FuncTag;

    if (type && type->isStruct())
        return Layout_PawnStruct;

    if (Type* type = gTypes.find(tag)) {
      if (findglb(type->name()))
          return Layout_Enum;
    }

    return Layout_None;
}

LayoutSpec
deduce_layout_spec_by_name(const char* name)
{
    Type* type = gTypes.find(name);
    if (!type)
        return Layout_None;

    return deduce_layout_spec_by_tag(type->tagid());
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
