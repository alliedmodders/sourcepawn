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
    char name[METHOD_NAMEMAX + 1];
    ke::Vector<functag_t*> entries;
};

struct structarg_t {
    structarg_t()
     : tag(0),
       name(nullptr),
       fconst(0),
       ident(0),
       offs(0),
       index(0)
    {}

    int tag;
    sp::Atom* name;
    int fconst;
    int ident;
    unsigned int offs;
    int index;
};

struct pstruct_t {
    explicit pstruct_t(const char* name);

    char name[sNAMEMAX + 1];
    ke::Vector<std::unique_ptr<structarg_t>> args;
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

    char name[METHOD_NAMEMAX + 1];
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
        if (thisp->ident == 0)
            return pc_tag_void;
        arginfo* valp = &setter->function()->args[1];
        if (valp->ident != iVARIABLE)
            return pc_tag_void;
        return valp->tag;
    }
};

struct methodmap_t {
    methodmap_t(methodmap_t* parent, LayoutSpec spec, const char* name);

    methodmap_t* parent;
    int tag;
    bool nullable;
    bool keyword_nullable;
    LayoutSpec spec;
    char name[sNAMEMAX + 1];
    ke::Vector<std::unique_ptr<methodmap_method_t>> methods;

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
pstruct_t* pstructs_add(const char* name);
void pstructs_free();
pstruct_t* pstructs_find(const char* name);
structarg_t* pstructs_addarg(pstruct_t* pstruct, const structarg_t* arg);
const structarg_t* pstructs_getarg(const pstruct_t* pstruct, sp::Atom* name);

/**
 * Function enumeration tags
 */
void funcenums_free();
funcenum_t* funcenums_add(const char* name);
void functags_add(funcenum_t* en, functag_t* src);
funcenum_t* funcenum_for_symbol(symbol* sym);
functag_t* functag_find_intrinsic(int tag);

/**
 * Given a name or tag, find any extra weirdness it has associated with it.
 */
LayoutSpec deduce_layout_spec_by_tag(int tag);
LayoutSpec deduce_layout_spec_by_name(const char* name);
const char* layout_spec_name(LayoutSpec spec);
bool can_redef_layout_spec(LayoutSpec olddef, LayoutSpec newdef);

/**
 * Heap functions
 */
void pushheaplist();
void popheaplist(bool codegen);
int markheap(int type, int size);

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
methodmap_t* methodmap_add(methodmap_t* parent, LayoutSpec spec, const char* name);
methodmap_t* methodmap_find_by_name(const char* name);
methodmap_method_t* methodmap_find_method(methodmap_t* map, const char* name);
void methodmaps_free();

#endif //_INCLUDE_SOURCEPAWN_COMPILER_TRACKER_H_
