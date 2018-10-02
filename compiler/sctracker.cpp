/* vim: set ts=8 sts=2 sw=2 tw=99 et: */
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>
#include <amtl/am-vector.h>
#include "sc.h"
#include "sctracker.h"
#include "types.h"

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
funcenum_t *firstenum = NULL;
funcenum_t *lastenum = NULL;
pstruct_t *firststruct = NULL;
pstruct_t *laststruct = NULL;
methodmap_t *methodmap_first = NULL;
methodmap_t *methodmap_last = NULL;

structarg_t *pstructs_getarg(pstruct_t *pstruct, const char *member)
{
  int i;

  for (i=0; i<pstruct->argcount; i++) {
    if (strcmp(pstruct->args[i]->name, member) == 0)
      return pstruct->args[i];
  }

  return NULL;
}

pstruct_t *pstructs_add(const char *name)
{
  pstruct_t *p = (pstruct_t *)malloc(sizeof(pstruct_t));
  
  memset(p, 0, sizeof(pstruct_t));
  strcpy(p->name, name);
  
  if (!firststruct) {
    firststruct = p;
    laststruct = p;
  } else {
    laststruct->next = p;
    laststruct = p;
  }

  return p;
}

void pstructs_free()
{
  pstruct_t *p, *next;

  p = firststruct;
  while (p) {
    while (p->argcount--)
      free(p->args[p->argcount]);
    free(p->args);
    next = p->next;
    free(p);
    p = next;
  }
  firststruct = NULL;
  laststruct = NULL;
}

pstruct_t *pstructs_find(const char *name)
{
  pstruct_t *p = firststruct;

  while (p) {
    if (strcmp(p->name, name) == 0)
      return p;
    p = p->next;
  }

  return NULL;
}

structarg_t *pstructs_addarg(pstruct_t *pstruct, const structarg_t *arg)
{
  structarg_t *newarg;
  int i;

  for (i=0; i<pstruct->argcount; i++) {
    if (strcmp(pstruct->args[i]->name, arg->name) == 0) {
      /* Don't allow dup names */
      return NULL;
    }
  }
  
  newarg = (structarg_t *)malloc(sizeof(structarg_t));

  memcpy(newarg, arg, sizeof(structarg_t));

  if (pstruct->argcount == 0) {
    pstruct->args = (structarg_t **)malloc(sizeof(structarg_t *) * 1);
  } else {
    pstruct->args = (structarg_t **)realloc(
              pstruct->args,
              sizeof(structarg_t *) * (pstruct->argcount + 1));
  }

  newarg->offs = pstruct->argcount * sizeof(cell);
  newarg->index = pstruct->argcount;
  pstruct->args[pstruct->argcount++] = newarg;

  return newarg;
}

void funcenums_free()
{
  funcenum_t *e, *next;

  e = firstenum;
  while (e) {
    functag_t *tag, *nexttag;
    tag = e->first;
    while (tag) {
      nexttag = tag->next;
      free(tag);
      tag = nexttag;
    }
    next = e->next;
    free(e);
    e = next;
  }

  firstenum = NULL;
  lastenum = NULL;
}

funcenum_t *funcenums_add(const char *name)
{
  funcenum_t *e = (funcenum_t *)malloc(sizeof(funcenum_t));

  memset(e, 0, sizeof(funcenum_t));

  if (!firstenum) {
    firstenum = e;
    lastenum = e;
  } else {
    lastenum->next = e;
    lastenum = e;
  }

  strcpy(e->name, name);
  e->tag = gTypes.defineFunction(name, e)->tagid();

  return e;
}

funcenum_t *funcenum_for_symbol(symbol *sym)
{
  functag_t ft;
  memset(&ft, 0, sizeof(ft));

  ft.ret_tag = sym->tag;
  ft.usage = uPUBLIC & (sym->usage & uRETVALUE);
  ft.argcount = 0;
  ft.ommittable = FALSE;
  for (arginfo& arg : sym->function()->args) {
    if (!arg.ident)
      break;

    funcarg_t *dest = &ft.args[ft.argcount++];

    dest->tagcount = 1;
    dest->tags[0] = arg.tag;

    dest->dimcount = arg.numdim;
    memcpy(dest->dims, arg.dim, arg.numdim * sizeof(int));

    dest->ident = arg.ident;
    dest->fconst = !!(arg.usage & uCONST);
    dest->ommittable = FALSE;
  }

  char name[METHOD_NAMEMAX+1];
  UTIL_Format(name, sizeof(name), "::ft:%s:%d:%d", sym->name(), sym->addr(), sym->codeaddr);

  funcenum_t *fe = funcenums_add(name);
  functags_add(fe, &ft);

  return fe;
}

// Finds a functag that was created intrinsically.
functag_t *functag_find_intrinsic(int tag)
{
  Type* type = gTypes.find(tag);
  funcenum_t *fe = type->asFunction();
  if (!fe)
    return NULL;

  if (strncmp(fe->name, "::ft:", 5) != 0)
    return NULL;

  assert(fe->first && fe->first == fe->last);
  return fe->first;
}

functag_t *functags_add(funcenum_t *en, functag_t *src)
{
  functag_t *t = (functag_t *)malloc(sizeof(functag_t));
  
  memcpy(t, src, sizeof(functag_t));

  t->next = NULL;

  if (en->first == NULL) {
    en->first = t;
    en->last = t;
  } else {
    en->last->next = t;
    en->last = t;
  }

  return t;
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

methodmap_t*
methodmap_add(methodmap_t* parent,
              LayoutSpec spec,
              const char* name)
{
  methodmap_t *map = (methodmap_t *)calloc(1, sizeof(methodmap_t));
  map->parent = parent;
  map->spec = spec;
  strcpy(map->name, name);

  if (spec == Layout_MethodMap && parent) {
    if (parent->nullable)
      map->nullable = parent->nullable;
    if (parent->keyword_nullable)
      map->keyword_nullable = parent->keyword_nullable;
  }

  if (!methodmap_first) {
    methodmap_first = map;
    methodmap_last = map;
  } else {
    methodmap_last->next = map;
    methodmap_last = map;
  }

  if (spec == Layout_MethodMap)
    map->tag = gTypes.defineMethodmap(name, map)->tagid();
  else
    map->tag = gTypes.defineObject(name)->tagid();
  return map;
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
  size_t i;
  for (i = 0; i < map->nummethods; i++) {
    if (strcmp(map->methods[i]->name, name) == 0)
      return map->methods[i];
  }
  if (map->parent)
    return methodmap_find_method(map->parent, name);
  return NULL;
}

void methodmap_add_method(methodmap_t* map, methodmap_method_t* method)
{
  methodmap_method_t** methods =
    (methodmap_method_t**)realloc(map->methods, sizeof(methodmap_method_t*) * (map->nummethods + 1));
  if (!methods) {
    error(FATAL_ERROR_OOM);
    return;
  }
  method->parent = map;
  map->methods = methods;
  map->methods[map->nummethods++] = method;
}

void methodmaps_free()
{
  methodmap_t *ptr = methodmap_first;
  while (ptr) {
    methodmap_t *next = ptr->next;
    for (size_t i = 0; i < ptr->nummethods; i++)
      free(ptr->methods[i]);
    free(ptr->methods);
    free(ptr);
    ptr = next;
  }
  methodmap_first = NULL;
  methodmap_last = NULL;
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

size_t UTIL_Format(char *buffer, size_t maxlength, const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  size_t len = vsnprintf(buffer, maxlength, fmt, ap);
  va_end(ap);

  if (len >= maxlength) {
    buffer[maxlength - 1] = '\0';
    return maxlength - 1;
  }
  return len;
}
