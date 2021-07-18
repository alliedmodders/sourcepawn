// vim: set ts=8 sts=4 sw=4 tw=99 et:
//  Pawn compiler - Recursive descend expresion parser
//
//  Copyright (c) ITB CompuPhase, 1997-2005
//  Copyright (c) AlliedModders LLC 2021
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
#include "symbols.h"

#include "array-helpers.h"
#include "lexer.h"
#include "sc.h"
#include "sclist.h"
#include "scvars.h"
#include "sp_symhash.h"

static symbol* sScopeChain;
static std::vector<symbol*> sAllocatedScopes;

AutoEnterScope::AutoEnterScope(symbol* scope)
{
    assert(scope->parent() == sScopeChain);
    sScopeChain = scope;
}

AutoEnterScope::~AutoEnterScope()
{
    sScopeChain = sScopeChain->parent();
}

symbol*
CreateScope()
{
    symbol* scope = new symbol();

    scope->ident = iSCOPE;
    scope->set_parent(sScopeChain);

    sAllocatedScopes.emplace_back(scope);
    return scope;
}

symbol*
GetScopeChain()
{
    if (sScopeChain)
        return sScopeChain;
    return &loctab;
}

symbol*
findconst(const char* name)
{
    symbol* sym;

    sym = findloc(name);          /* try local symbols first */
    if (sym == NULL || sym->ident != iCONSTEXPR) { /* not found, or not a constant */
        sym = FindInHashTable(sp_Globals, name, fcurrent);
    }
    if (sym == NULL || sym->ident != iCONSTEXPR)
        return NULL;
    assert(sym->parent() == NULL || sym->enumfield);
    /* ^^^ constants have no hierarchy, but enumeration fields may have a parent */
    return sym;
}

/*  findglb
 *
 *  Returns a pointer to the global symbol (if found) or NULL (if not found)
 */
symbol*
findglb(const char* name)
{
    return FindInHashTable(sp_Globals, name, fcurrent);
}

/*  findloc
 *
 *  Returns a pointer to the local symbol (if found) or NULL (if not found).
 *  See add_symbol() how the deepest nesting level is searched first.
 */
symbol*
table_findloc(symbol* table, sp::Atom* atom)
{
    symbol* sym = table->next;
    while (sym != NULL) {
        if (atom == sym->nameAtom() &&
            (sym->parent() == NULL ||
             sym->ident ==
                 iCONSTEXPR)) /* sub-types (hierarchical types) are skipped, except for enum fields */
        {
            return sym; /* return first match */
        }
        sym = sym->next;
    }
    return nullptr;
}

symbol*
findloc(const char* name, symbol** scope)
{
    sp::Atom* atom = gAtoms.add(name);
    for (symbol* iter = sScopeChain; iter; iter = iter->parent()) {
        if (symbol* sym = table_findloc(iter, atom)) {
            if (scope)
                *scope = iter;
            return sym;
        }
    }
    if (scope)
        *scope = &loctab;
    return table_findloc(&loctab, atom);
}

/* The local variable table must be searched backwards, so that the deepest
 * nesting of local variables is searched first. The simplest way to do
 * this is to insert all new items at the head of the list.
 * In the global list, the symbols are kept in sorted order, so that the
 * public functions are written in sorted order.
 */
static symbol*
add_symbol(symbol* root, symbol* entry)
{
    entry->next = root->next;
    root->next = entry;
    if (root == &glbtab)
        AddToHashTable(sp_Globals, entry);
    return entry;
}

static void
free_symbol(symbol* sym)
{
    delete sym;
}

void
delete_symbol(symbol* root, symbol* sym)
{
    symbol* origRoot = root;
    /* find the symbol and its predecessor
     * (this function assumes that you will never delete a symbol that is not
     * in the table pointed at by "root")
     */
    assert(root != sym);
    while (root->next != sym) {
        root = root->next;
        assert(root != NULL);
    }

    if (origRoot == &glbtab)
        RemoveFromHashTable(sp_Globals, sym);

    /* unlink it, then free it */
    root->next = sym->next;
    free_symbol(sym);
}

void
delete_symbols(symbol* root, int delete_functions)
{
    symbol* origRoot = root;
    symbol *sym, *parent_sym;
    int mustdelete;

    /* erase only the symbols with a deeper nesting level than the
     * specified nesting level */
    while (root->next != NULL) {
        sym = root->next;
        switch (sym->ident) {
            case iVARIABLE:
            case iARRAY:
                /* do not delete global variables if functions are preserved */
                mustdelete = delete_functions;
                break;
            case iREFERENCE:
                /* always delete references (only exist as function parameters) */
                mustdelete = TRUE;
                break;
            case iREFARRAY:
                /* a global iREFARRAY symbol is the return value of a function: delete
                 * this only if "globals" must be deleted; other iREFARRAY instances
                 * (locals) are also deleted
                 */
                mustdelete = delete_functions;
                for (parent_sym = sym->parent(); parent_sym != NULL && parent_sym->ident != iFUNCTN;
                     parent_sym = parent_sym->parent())
                    assert(parent_sym->ident == iREFARRAY);
                assert(parent_sym == NULL ||
                       (parent_sym->ident == iFUNCTN && parent_sym->parent() == NULL));
                if (parent_sym == NULL || parent_sym->ident != iFUNCTN)
                    mustdelete = TRUE;
                break;
            case iCONSTEXPR:
            case iENUMSTRUCT:
                /* delete constants, except predefined constants */
                mustdelete = delete_functions || !sym->predefined;
                break;
            case iFUNCTN:
                /* optionally preserve globals (variables & functions), but
                 * NOT native functions
                 */
                mustdelete = delete_functions || sym->native;
                assert(sym->parent() == NULL);
                break;
            case iMETHODMAP:
                // We delete methodmap symbols at the end, but since methodmaps
                // themselves get wiped, we null the pointer.
                sym->methodmap = nullptr;
                mustdelete = delete_functions;
                assert(!sym->parent());
                break;
            case iARRAYCELL:
            case iARRAYCHAR:
            case iEXPRESSION:
            case iVARARGS:
            case iACCESSOR:
            default:
                assert(0);
                break;
        }
        if (mustdelete) {
            if (origRoot == &glbtab)
                RemoveFromHashTable(sp_Globals, sym);
            root->next = sym->next;
            free_symbol(sym);
        } else {
            /* if the function was prototyped, but not implemented in this source,
             * mark it as such, so that its use can be flagged
             */
            if (sym->ident == iFUNCTN && !sym->defined)
                sym->missing = true;
            if (sym->ident == iFUNCTN || sym->ident == iVARIABLE || sym->ident == iARRAY)
                sym->defined = false;
            /* for user defined operators, also remove the "prototyped" flag, as
             * user-defined operators *must* be declared before use
             */
            if (sym->ident == iFUNCTN && !alpha(*sym->name()))
                sym->prototyped = false;
            if (origRoot == &glbtab)
                sym->clear_refers();
            root = sym; /* skip the symbol */
        }
    }
}

void
markusage(symbol* sym, int usage)
{
    // When compiling a skipped function, do not accumulate liveness information
    // for referenced functions.
    if (sc_status == statSKIP && sym->ident == iFUNCTN)
        return;

    sym->usage |= usage;
    /* check if (global) reference must be added to the symbol */
    if ((usage & (uREAD | uWRITTEN)) != 0) {
        /* only do this for global symbols */
        if (sym->vclass == sGLOBAL && curfunc)
            curfunc->add_reference_to(sym);
    }
}

FunctionData::FunctionData()
 : funcid(0),
   dbgstrs(nullptr),
   array(nullptr)
{
    resizeArgs(0);
}

FunctionData::~FunctionData() {
    if (dbgstrs) {
        delete_stringtable(dbgstrs);
        free(dbgstrs);
    }
    delete array;
}

void
FunctionData::resizeArgs(size_t nargs)
{
    args.resize(nargs);
    args.emplace_back();
}

symbol::symbol()
 : symbol("", 0, 0, 0, 0)
{}

symbol::symbol(const char* symname, cell symaddr, int symident, int symvclass, int symtag)
 : next(nullptr),
   codeaddr(code_idx),
   vclass((char)symvclass),
   ident((char)symident),
   tag(symtag),
   usage(0),
   defined(false),
   is_const(false),
   stock(false),
   is_public(false),
   is_static(false),
   is_struct(false),
   prototyped(false),
   missing(false),
   callback(false),
   skipped(false),
   retvalue(false),
   forward(false),
   native(false),
   retvalue_used(false),
   enumroot(false),
   enumfield(false),
   predefined(false),
   deprecated(false),
   queued(false),
   explicit_return_type(false),
   x({}),
   fnumber(fcurrent),
   /* assume global visibility (ignored for local symbols) */
   lnumber(fline),
   methodmap(nullptr),
   addr_(symaddr),
   name_(nullptr),
   referred_from_count_(0),
   parent_(nullptr),
   child_(nullptr)
{
    if (symname)
        name_ = gAtoms.add(symname);
    if (symident == iFUNCTN)
        data_.reset(new FunctionData);
    memset(&dim, 0, sizeof(dim));
}

symbol::symbol(const symbol& other)
 : symbol(nullptr, other.addr_, other.ident, other.vclass, other.tag)
{
    name_ = other.name_;

    usage = other.usage;
    defined = other.defined;
    prototyped = other.prototyped;
    missing = other.missing;
    enumroot = other.enumroot;
    enumfield = other.enumfield;
    predefined = other.predefined;
    callback = other.callback;
    skipped = other.skipped;
    retvalue = other.retvalue;
    forward = other.forward;
    native = other.native;
    stock = other.stock;
    is_struct = other.is_struct;
    is_public = other.is_public;
    is_const = other.is_const;
    deprecated = other.deprecated;
    // Note: explicitly don't add queued.

    x = other.x;
}

symbol::~symbol()
{
    if (ident == iCONSTEXPR && enumroot) {
        /* free the constant list of an enum root */
        assert(dim.enumlist != NULL);
        delete_consttable(dim.enumlist);
        free(dim.enumlist);
    }
}

void
symbol::add_reference_to(symbol* other)
{
    for (symbol* sym : refers_to_) {
        if (sym == other)
            return;
    }
    refers_to_.push_back(other);
    other->referred_from_.push_back(this);
    other->referred_from_count_++;
}

void
symbol::drop_reference_from(symbol* from)
{
#if !defined(NDEBUG)
    bool found = false;
    for (size_t i = 0; i < referred_from_.size(); i++) {
        if (referred_from_[i] == from) {
            referred_from_[i] = nullptr;
            found = true;
            break;
        }
    }
    assert(found);
#endif
    referred_from_count_--;
}

bool
symbol::must_return_value() const
{
    assert(ident == iFUNCTN);
    return retvalue || (explicit_return_type && tag != pc_tag_void);
}

/*  addsym
 *
 *  Adds a symbol to the symbol table (either global or local variables,
 *  or global and local constants).
 */
symbol*
addsym(const char* name, cell addr, int ident, int vclass, int tag)
{
    /* first fill in the entry */
    symbol* sym = new symbol(name, addr, ident, vclass, tag);

    /* then insert it in the list */
    if (vclass == sGLOBAL)
        return add_symbol(&glbtab, sym);
    return add_symbol(GetScopeChain(), sym);
}

symbol*
addvariable(const char* name, cell addr, int ident, int vclass, int tag, int dim[], int numdim,
            int idxtag[])
{
    symbol* sym;

    /* global variables may only be defined once
     * One complication is that functions returning arrays declare an array
     * with the same name as the function, so the assertion must allow for
     * this special case. Another complication is that variables may be
     * "redeclared" if they are local to an automaton (and findglb() will find
     * the symbol without states if no symbol with states exists).
     */
    assert(vclass != sGLOBAL || (sym = findglb(name)) == NULL || !sym->defined ||
           (sym->ident == iFUNCTN && sym == curfunc));

    if (ident == iARRAY || ident == iREFARRAY) {
        symbol *parent = NULL, *top;
        int level;
        sym = NULL; /* to avoid a compiler warning */
        for (level = 0; level < numdim; level++) {
            top = addsym(name, addr, ident, vclass, tag);
            top->defined = true;
            top->dim.array.length = dim[level];
            top->dim.array.level = (short)(numdim - level - 1);
            top->x.tags.index = idxtag[level];
            top->set_parent(parent);
            if (parent) {
                parent->set_array_child(top);
            }
            parent = top;
            if (level == 0)
                sym = top;
        }
    } else {
        sym = addsym(name, addr, ident, vclass, tag);
        sym->defined = true;
    }
    return sym;
}

