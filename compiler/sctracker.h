/* vim: set sts=2 ts=8 sw=2 tw=99 et: */
#ifndef _INCLUDE_SOURCEPAWN_COMPILER_TRACKER_H_
#define _INCLUDE_SOURCEPAWN_COMPILER_TRACKER_H_

#include <memory>

#include "lexer.h"
#include "pool-allocator.h"
#include "scvars.h"

#define MEMUSE_STATIC 0
#define MEMUSE_DYNAMIC 1

struct funcenum_t {
    funcenum_t()
     : tag(0),
       name()
    {}
    int tag;
    sp::Atom* name;
    std::vector<functag_t*> entries;
};

struct structarg_t {
    structarg_t()
      : type(),
        name(nullptr),
        offs(0),
        index(0)
    {}

    typeinfo_t type;
    sp::Atom* name;
    unsigned int offs;
    int index;
};

struct pstruct_t {
    explicit pstruct_t(sp::Atom* name);

    sp::Atom* name;
    std::vector<std::unique_ptr<structarg_t>> args;
};

// The ordering of these definitions should be preserved for
// can_redef_layout_spec().
typedef enum LayoutSpec_t {
    Layout_None,
    Layout_Enum,
    Layout_FuncTag,
    Layout_PawnStruct,
    Layout_MethodMap,
    Layout_Class
} LayoutSpec;

struct methodmap_method_t {
    explicit methodmap_method_t(methodmap_t* parent)
     : name(),
       parent(parent),
       target(nullptr),
       getter(nullptr),
       setter(nullptr),
       is_static(false)
    {}

    sp::Atom* name;
    methodmap_t* parent;
    symbol* target;
    symbol* getter;
    symbol* setter;
    bool is_static;

    int property_tag() const {
        assert(getter || setter);
        if (getter)
            return getter->tag;
        arginfo* thisp = &setter->function()->args[0];
        if (thisp->type.ident == 0)
            return pc_tag_void;
        arginfo* valp = &setter->function()->args[1];
        if (valp->type.ident != iVARIABLE)
            return pc_tag_void;
        return valp->type.tag;
    }
};

struct methodmap_t : public SymbolData {
    methodmap_t(methodmap_t* parent, LayoutSpec spec, sp::Atom* name);

    methodmap_t* asMethodmap() override { return this; }

    methodmap_t* parent;
    int tag;
    bool nullable;
    bool keyword_nullable;
    LayoutSpec spec;
    sp::Atom* name;
    std::vector<std::unique_ptr<methodmap_method_t>> methods;

    bool must_construct_with_new() const {
        return nullable || keyword_nullable;
    }

    // Shortcut.
    methodmap_method_t* dtor;
    methodmap_method_t* ctor;
};

/**
 * Pawn Structs
 */
pstruct_t* pstructs_add(sp::Atom* name);
void pstructs_free();
pstruct_t* pstructs_find(const char* name);
structarg_t* pstructs_addarg(pstruct_t* pstruct, const structarg_t* arg);
const structarg_t* pstructs_getarg(const pstruct_t* pstruct, sp::Atom* name);

/**
 * Function enumeration tags
 */
void funcenums_free();
funcenum_t* funcenums_add(sp::Atom* name);
void functags_add(funcenum_t* en, functag_t* src);
funcenum_t* funcenum_for_symbol(symbol* sym);
functag_t* functag_from_tag(int tag);

/**
 * Given a name or tag, find any extra weirdness it has associated with it.
 */
LayoutSpec deduce_layout_spec_by_tag(int tag);
LayoutSpec deduce_layout_spec_by_name(const char* name);
const char* layout_spec_name(LayoutSpec spec);
bool can_redef_layout_spec(LayoutSpec olddef, LayoutSpec newdef);

enum class AllocScopeKind {
  Normal,
  Temp
};

/**
 * Heap functions
 */
void pushheaplist(AllocScopeKind kind = AllocScopeKind::Normal);
void popheaplist(bool codegen);
int markheap(int type, int size, AllocScopeKind kind);

// Remove the current heap scope, requiring that all alocations within be
// static. Then return that static size.
cell_t pop_static_heaplist();

/**
 * Stack functions
 */
void pushstacklist();
void popstacklist(bool codegen);
int markstack(int type, int size);
int stack_scope_id();
int heap_scope_id();
bool has_stack_or_heap_scopes();

/**
 * Generates code to free mem usage, but does not pop the list.  
 *  This is used for code like dobreak()/docont()/doreturn().
 * stop_id is the list at which to stop generating.
 */
void genstackfree(int stop_id);
void genheapfree(int stop_id);

/**
 * Resets a mem list by freeing everything
 */
void resetstacklist();
void resetheaplist();

/**
 * Method maps.
 */
methodmap_t* methodmap_add(methodmap_t* parent, LayoutSpec spec, sp::Atom* name);
methodmap_t* methodmap_find_by_name(sp::Atom* name);
methodmap_method_t* methodmap_find_method(methodmap_t* map, sp::Atom* name);
void methodmaps_free();

#endif //_INCLUDE_SOURCEPAWN_COMPILER_TRACKER_H_
