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
#include "errors.h"
#include "lexer.h"
#include "parser.h"
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
    return sScopeChain;
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

symbol*
findglb(sp::Atom* name)
{
    return findglb(name->chars());
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
    auto atom = gAtoms.add(name);
    return findloc(atom, scope);
}

symbol*
findloc(sp::Atom* name, symbol** scope)
{
    for (symbol* iter = sScopeChain; iter; iter = iter->parent()) {
        if (symbol* sym = table_findloc(iter, name)) {
            if (scope)
                *scope = iter;
            return sym;
        }
    }
    return nullptr;
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
            int semantic_tag)
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
            top->x.tags.index = (level == numdim - 1) ? semantic_tag : 0;
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

int
findnamedarg(arginfo* arg, sp::Atom* name)
{
    int i;

    for (i = 0; arg[i].type.ident != 0 && arg[i].type.ident != iVARARGS; i++)
        if (arg[i].name == name)
            return i;
    return -1;
}

symbol*
find_enumstruct_field(Type* type, sp::Atom* name)
{
    assert(type->asEnumStruct());

    auto const_name = ke::StringPrintf("%s::%s", type->name(), name->chars());
    auto atom = gAtoms.add(const_name);
    if (symbol* sym = findconst(atom->chars()))
        return sym;
    return findglb(atom);
}

static char*
tag2str(char* dest, int tag)
{
    assert(tag >= 0);
    sprintf(dest, "0%x", tag);
    return isdigit(dest[1]) ? &dest[1] : dest;
}

sp::Atom*
operator_symname(const char* opername, int tag1, int tag2, int numtags, int resulttag)
{
    char tagstr1[10], tagstr2[10];
    int opertok;

    assert(numtags >= 1 && numtags <= 2);
    opertok = (opername[1] == '\0') ? opername[0] : 0;

    std::string symname;
    if (opertok == '=')
        symname = ke::StringPrintf("%s%s%s", tag2str(tagstr1, resulttag), opername, tag2str(tagstr2, tag1));
    else if (numtags == 1 || opertok == '~')
        symname = ke::StringPrintf("%s%s", opername, tag2str(tagstr1, tag1));
    else
        symname = ke::StringPrintf("%s%s%s", tag2str(tagstr1, tag1), opername, tag2str(tagstr2, tag2));
    return gAtoms.add(symname);
}

/*
 *  Finds a function in the global symbol table or creates a new entry.
 *  It does some basic processing and error checking.
 */
symbol*
fetchfunc(const char* name)
{
    symbol* sym;

    if ((sym = findglb(name)) != 0) { /* already in symbol table? */
        if (sym->ident != iFUNCTN) {
            error(21, name); /* yes, but not as a function */
            return NULL;     /* make sure the old symbol is not damaged */
        } else if (sym->native) {
            error(21, name); /* yes, and it is a native */
        }
        assert(sym->vclass == sGLOBAL);
    } else {
        /* don't set the "uDEFINE" flag; it may be a prototype */
        sym = addsym(name, code_idx, iFUNCTN, sGLOBAL, 0);
        assert(sym != NULL); /* fatal error 103 must be given on error */
    }
    if (pc_deprecate.size() > 0) {
        assert(sym != NULL);
        sym->deprecated = true;
        if (sc_status == statWRITE) {
            sym->documentation = std::move(pc_deprecate);
        } else {
            pc_deprecate = "";
        }
    }

    return sym;
}

int
check_operatortag(int opertok, int resulttag, const char* opername)
{
    assert(opername != NULL && strlen(opername) > 0);
    switch (opertok) {
        case '!':
        case '<':
        case '>':
        case tlEQ:
        case tlNE:
        case tlLE:
        case tlGE:
            if (resulttag != pc_tag_bool) {
                error(63, opername, "bool:"); /* operator X requires a "bool:" result tag */
                return FALSE;
            }
            break;
        case '~':
            if (resulttag != 0) {
                error(63, opername, "_:"); /* operator "~" requires a "_:" result tag */
                return FALSE;
            }
            break;
    }
    return TRUE;
}

static int
parse_funcname(const char* fname, int* tag1, int* tag2, char* opname, size_t opname_len)
{
    const char* ptr;
    int unary;

    /* tags are only positive, so if the function name starts with a '-',
     * the operator is an unary '-' or '--' operator.
     */
    if (*fname == '-') {
        *tag1 = 0;
        unary = TRUE;
        ptr = fname;
    } else {
        *tag1 = (int)strtol(fname, (char**)&ptr, 16);
        unary = ptr == fname; /* unary operator if it doesn't start with a tag name */
    }
    assert(!unary || *tag1 == 0);
    assert(*ptr != '\0');
    size_t chars_to_copy = 0;
    for (const char* iter = ptr; *iter && !isdigit(*iter); iter++)
        chars_to_copy++;
    ke::SafeStrcpyN(opname, opname_len, ptr, chars_to_copy);
    *tag2 = (int)strtol(&ptr[chars_to_copy], NULL, 16);
    return unary;
}

std::string
funcdisplayname(const char* funcname)
{
    int tags[2];
    char opname[10];
    int unary;

    if (isalpha(*funcname) || *funcname == '_' || *funcname == PUBLIC_CHAR || *funcname == '\0')
        return funcname;

    unary = parse_funcname(funcname, &tags[0], &tags[1], opname, sizeof(opname));
    Type* rhsType = gTypes.find(tags[1]);
    assert(rhsType != NULL);
    if (unary) {
        return ke::StringPrintf("operator%s(%s:)", opname, rhsType->name());
    }

    Type* lhsType = gTypes.find(tags[0]);
    assert(lhsType != NULL);
    /* special case: the assignment operator has the return value as the 2nd tag */
    if (opname[0] == '=' && opname[1] == '\0')
        return ke::StringPrintf("%s:operator%s(%s:)", lhsType->name(), opname, rhsType->name());
    return ke::StringPrintf("operator%s(%s:,%s:)", opname, lhsType->name(), rhsType->name());
}

static inline bool
is_symbol_unused(symbol* sym)
{
    if (sym->parent())
        return false;
    if (!sym->is_unreferenced())
        return false;
    if (sym->is_public)
        return false;
    if (sym->ident == iVARIABLE || sym->ident == iARRAY)
        return true;
    return sym->ident == iFUNCTN && !sym->native &&
           strcmp(sym->name(), uMAINFUNC) != 0;
}

void
reduce_referrers(symbol* root)
{
    std::vector<symbol*> work;

    // Enqueue all unreferred symbols.
    for (symbol* sym = root->next; sym; sym = sym->next) {
        if (is_symbol_unused(sym)) {
            sym->queued = true;
            work.push_back(sym);
        }
    }

    while (!work.empty()) {
        symbol* dead = ke::PopBack(&work);
        dead->usage &= ~(uREAD | uWRITTEN);

        for (symbol* sym : dead->refers_to()) {
            sym->drop_reference_from(dead);
            if (is_symbol_unused(sym) && !sym->queued) {
                // During compilation, anything marked as stock will be omitted from
                // the final binary *without warning*. If a stock calls a non-stock
                // function, we want to avoid warnings on that function as well, so
                // we propagate the stock bit.
                if (dead->stock)
                    sym->stock = true;

                sym->queued = true;
                work.push_back(sym);
            }
        }
    }
}

// Determine the set of live functions. Note that this must run before delete_symbols,
// since that resets referrer lists.
void
deduce_liveness(symbol* root)
{
    std::vector<symbol*> work;

    // The root set is all public functions.
    for (symbol* sym = root->next; sym; sym = sym->next) {
        if (sym->ident != iFUNCTN)
            continue;
        if (sym->native)
            continue;

        if (sym->is_public) {
            sym->queued = true;
            work.push_back(sym);
        } else {
            sym->queued = false;
        }
    }

    // Traverse referrers to find the transitive set of live functions.
    while (!work.empty()) {
        symbol* live = ke::PopBack(&work);

        for (const auto& other : live->refers_to()) {
            if (other->ident != iFUNCTN || other->queued)
                continue;
            other->queued = true;
            work.push_back(other);
        }
    }

    // Remove the liveness flags for anything we did not visit.
    for (symbol* sym = root->next; sym; sym = sym->next) {
        if (sym->ident != iFUNCTN || sym->queued)
            continue;
        if (sym->native)
            continue;
        sym->usage &= ~(uWRITTEN | uREAD);
    }
}

static constvalue*
insert_constval(constvalue* prev, constvalue* next, sp::Atom* name, cell val, int index)
{
    constvalue* cur;

    if ((cur = (constvalue*)malloc(sizeof(constvalue))) == NULL)
        error(FATAL_ERROR_OOM); /* insufficient memory (fatal error) */
    memset(cur, 0, sizeof(constvalue));
    cur->name = name;
    cur->value = val;
    cur->index = index;
    cur->next = next;
    prev->next = cur;
    return cur;
}

constvalue*
append_constval(constvalue* table, sp::Atom* name, cell val, int index)
{
    constvalue *cur, *prev;

    /* find the end of the constant table */
    for (prev = table, cur = table->next; cur != NULL; prev = cur, cur = cur->next)
        /* nothing */;
    return insert_constval(prev, nullptr, name, val, index);
}

void
delete_consttable(constvalue* table)
{
    constvalue *cur = table->next, *next;

    while (cur != NULL) {
        next = cur->next;
        free(cur);
        cur = next;
    }
    memset(table, 0, sizeof(constvalue));
}

/*  add_constant
 *
 *  Adds a symbol to the symbol table. Returns NULL on failure.
 */
symbol*
add_constant(const char* name, cell val, int vclass, int tag)
{
    symbol* sym;

    /* Test whether a global or local symbol with the same name exists. Since
     * constants are stored in the symbols table, this also finds previously
     * defind constants. */
    sym = findglb(name);
    if (!sym)
        sym = findloc(name);
    if (sym) {
        int redef = 0;
        if (sym->ident != iCONSTEXPR)
            redef = 1; /* redefinition a function/variable to a constant is not allowed */
        if (sym->enumfield) {
            /* enum field, special case if it has a different tag and the new symbol is also an enum field */
            symbol* tagsym;
            if (sym->tag == tag)
                redef = 1; /* enumeration field is redefined (same tag) */
            Type* type = gTypes.find(tag);
            if (type == NULL) {
                redef = 1; /* new constant does not have a tag */
            } else {
                tagsym = findconst(type->name());
                if (tagsym == NULL || !tagsym->enumroot)
                    redef = 1; /* new constant is not an enumeration field */
            }
            /* in this particular case (enumeration field that is part of a different
             * enum, and non-conflicting with plain constants) we want to be able to
             * redefine it
             */
            if (!redef)
                goto redef_enumfield;
        } else if (sym->tag != tag) {
            redef = 1; /* redefinition of a constant (non-enum) to a different tag is not allowed */
        }
        if (redef) {
            error(21, name); /* symbol already defined */
            return NULL;
        } else if (sym->addr() != val) {
            error(201, name);  /* redefinition of constant (different value) */
            sym->setAddr(val); /* set new value */
        }
        /* silently ignore redefinitions of constants with the same value & tag */
        return sym;
    }

    /* constant doesn't exist yet (or is allowed to be redefined) */
redef_enumfield:
    sym = addsym(name, val, iCONSTEXPR, vclass, tag);
    sym->defined = true;
    if (sc_status == statIDLE)
        sym->predefined = true;
    return sym;
}

