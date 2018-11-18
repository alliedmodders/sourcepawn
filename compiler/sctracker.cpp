/* vim: set ts=8 sts=2 sw=2 tw=99 et: */
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>
#include <amtl/am-vector.h>
#include "sc.h"
#include "sctracker.h"
#include "codegen.h"
#include "types.h"
#include "lexer.h"

struct MemoryUse {
  MemoryUse(int type, int size)
   : type(type), size(size)
  {}
  int type;   /* MEMUSE_STATIC or MEMUSE_DYNAMIC */
  int size;   /* size of array for static (0 for dynamic) */
};

struct MemoryScope {
  explicit MemoryScope(int scope_id)
   : scope_id(scope_id)
  {}
  int scope_id;
  ke::Vector<MemoryUse> usage;
};

ke::Vector<MemoryScope> sStackScopes;
ke::Vector<MemoryScope> sHeapScopes;
ke::Vector<ke::UniquePtr<funcenum_t>> sFuncEnums;
ke::Vector<ke::UniquePtr<pstruct_t>> sStructs;
ke::Vector<ke::UniquePtr<methodmap_t>> sMethodmaps;

pstruct_t::pstruct_t(const char* name)
{
  ke::SafeStrcpy(this->name, sizeof(this->name), name);
}

structarg_t*
pstructs_getarg(pstruct_t *pstruct, const char *member)
{
  for (const auto& arg : pstruct->args) {
    if (strcmp(arg->name, member) == 0)
      return arg.get();
  }
  return nullptr;
}

pstruct_t*
pstructs_add(const char *name)
{
  auto p = ke::MakeUnique<pstruct_t>(name);
  sStructs.append(ke::Move(p));
  return sStructs.back().get();
}

void
pstructs_free()
{
  sStructs.clear();
}

pstruct_t*
pstructs_find(const char *name)
{
  for (const auto& p : sStructs) {
    if (strcmp(p->name, name) == 0)
      return p.get();
  }
  return nullptr;
}

structarg_t*
pstructs_addarg(pstruct_t *pstruct, const structarg_t *arg)
{
  if (pstructs_getarg(pstruct, arg->name))
    return nullptr;

  auto newarg = ke::MakeUnique<structarg_t>();
  memcpy(newarg.get(), arg, sizeof(structarg_t));
  newarg->offs = pstruct->args.length() * sizeof(cell);
  newarg->index = pstruct->args.length();
  pstruct->args.append(ke::Move(newarg));

  return pstruct->args.back().get();
}

void funcenums_free()
{
  sFuncEnums.clear();
}

funcenum_t *funcenums_add(const char *name)
{
  auto e = ke::MakeUnique<funcenum_t>();

  strcpy(e->name, name);
  e->tag = gTypes.defineFunction(name, e.get())->tagid();

  sFuncEnums.append(ke::Move(e));
  return sFuncEnums.back().get();
}

funcenum_t *funcenum_for_symbol(symbol *sym)
{
  auto ft = ke::MakeUnique<functag_t>();

  ft->ret_tag = sym->tag;
  ft->usage = uPUBLIC & (sym->usage & uRETVALUE);
  ft->argcount = 0;
  ft->ommittable = FALSE;
  for (arginfo& arg : sym->function()->args) {
    if (!arg.ident)
      break;

    funcarg_t *dest = &ft->args[ft->argcount++];

    dest->tagcount = 1;
    dest->tags[0] = arg.tag;

    dest->dimcount = arg.numdim;
    memcpy(dest->dims, arg.dim, arg.numdim * sizeof(int));

    dest->ident = arg.ident;
    dest->fconst = !!(arg.usage & uCONST);
    dest->ommittable = FALSE;
  }

  char name[METHOD_NAMEMAX+1];
  ke::SafeSprintf(name, sizeof(name), "::ft:%s:%d:%d", sym->name(), sym->addr(), sym->codeaddr);

  funcenum_t *fe = funcenums_add(name);
  functags_add(fe, ke::Move(ft));

  return fe;
}

// Finds a functag that was created intrinsically.
functag_t *functag_find_intrinsic(int tag)
{
  Type* type = gTypes.find(tag);
  funcenum_t *fe = type->asFunction();
  if (!fe)
    return nullptr;
  if (strncmp(fe->name, "::ft:", 5) != 0)
    return nullptr;
  if (fe->entries.empty())
    return nullptr;
  return fe->entries.back().get();
}

functag_t *functags_add(funcenum_t *en, ke::UniquePtr<functag_t> &&src)
{
  en->entries.append(ke::Move(src));
  return en->entries.back().get();
}

static void
EnterMemoryScope(ke::Vector<MemoryScope>& frame)
{
  if (frame.empty())
    frame.append(MemoryScope{0});
  else
    frame.append(MemoryScope{frame.back().scope_id + 1});
}

static void
AllocInScope(MemoryScope& scope, int type, int size)
{
  if (type == MEMUSE_STATIC &&
      !scope.usage.empty() &&
      scope.usage.back().type == MEMUSE_STATIC)
  {
    scope.usage.back().size += size;
  } else {
    scope.usage.append(MemoryUse{type, size});
  }
}

void
pushheaplist()
{
  EnterMemoryScope(sHeapScopes);
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
  sHeapScopes.pop();
  return total;
}

int
markheap(int type, int size)
{
  AllocInScope(sHeapScopes.back(), type, size);
  return size;
}

void
pushstacklist()
{
  EnterMemoryScope(sStackScopes);
}

int
markstack(int type, int size)
{
  AllocInScope(sStackScopes.back(), type, size);
  return size;
}

// Generates code to free all heap allocations on a tracker
static void
modheap_for_scope(const MemoryScope& scope)
{
  for (size_t i = scope.usage.length() - 1; i < scope.usage.length(); i--) {
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
  if (codegen)
    modheap_for_scope(sHeapScopes.back());
  sHeapScopes.pop();
}

void
genstackfree(int stop_id)
{
  for (size_t i = sStackScopes.length() - 1; i < sStackScopes.length(); i--) {
    const MemoryScope& scope = sStackScopes[i];
    if (scope.scope_id <= stop_id)
      break;
    modstk_for_scope(scope);
  }
}

void
genheapfree(int stop_id)
{
  for (size_t i = sHeapScopes.length() - 1; i < sHeapScopes.length(); i--) {
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
  sStackScopes.pop();
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

methodmap_t::methodmap_t(methodmap_t* parent, LayoutSpec spec, const char* name)
 : parent(parent),
   tag(0),
   nullable(false),
   keyword_nullable(false),
   spec(spec),
   dtor(nullptr),
   ctor(nullptr)
{
  ke::SafeStrcpy(this->name, sizeof(this->name), name);
}

methodmap_t*
methodmap_add(methodmap_t* parent, LayoutSpec spec, const char* name)
{
  auto map = ke::MakeUnique<methodmap_t>(parent, spec, name);

  if (spec == Layout_MethodMap && parent) {
    if (parent->nullable)
      map->nullable = parent->nullable;
    if (parent->keyword_nullable)
      map->keyword_nullable = parent->keyword_nullable;
  }

  if (spec == Layout_MethodMap)
    map->tag = gTypes.defineMethodmap(name, map.get())->tagid();
  else
    map->tag = gTypes.defineObject(name)->tagid();
  sMethodmaps.append(ke::Move(map));

  return sMethodmaps.back().get();
}

methodmap_t *methodmap_find_by_tag(int tag)
{
  return gTypes.find(tag)->asMethodmap();
}

methodmap_t *methodmap_find_by_name(const char *name)
{
  int tag = pc_findtag(name);
  if (tag == -1)
    return NULL;
  return methodmap_find_by_tag(tag);
}

methodmap_method_t *methodmap_find_method(methodmap_t *map, const char *name)
{
  for (const auto& method : map->methods) {
    if (strcmp(method->name, name) == 0)
      return method.get();
  }
  if (map->parent)
    return methodmap_find_method(map->parent, name);
  return nullptr;
}

void methodmaps_free()
{
  sMethodmaps.clear();
}

LayoutSpec deduce_layout_spec_by_tag(int tag)
{
  if (methodmap_t* map = methodmap_find_by_tag(tag))
    return map->spec;

  Type* type = gTypes.find(tag);
  if (type && type->isFunction())
    return Layout_FuncTag;

  if (type && type->isStruct())
    return Layout_PawnStruct;

  const char* name = pc_tagname(tag);
  if (findglb(name))
    return Layout_Enum;

  return Layout_None;
}

LayoutSpec deduce_layout_spec_by_name(const char *name)
{
  Type* type = gTypes.find(name);
  if (!type)
    return Layout_None;

  return deduce_layout_spec_by_tag(type->tagid());
}

const char *layout_spec_name(LayoutSpec spec)
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

