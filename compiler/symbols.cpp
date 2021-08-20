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
#include "compile-context.h"
#include "errors.h"
#include "lexer.h"
#include "parser.h"
#include "sc.h"
#include "sclist.h"
#include "scvars.h"
#include "semantics.h"

void
SymbolScope::Add(symbol* sym)
{
    assert(symbols_.find(sym->nameAtom()) == symbols_.end());
    symbols_.emplace(sym->nameAtom(), sym);
}

void
SymbolScope::AddChain(symbol* sym)
{
    auto iter = symbols_.find(sym->nameAtom());
    if (iter == symbols_.end()) {
        symbols_.emplace(sym->nameAtom(), sym);
    } else {
        sym->next = iter->second;
        iter->second = sym;
    }
#ifndef NDEBUG
    for (auto iter = sym->next; iter; iter = iter->next) {
        if (sym->is_static)
            assert(!iter->is_static || iter->fnumber != sym->fnumber);
        else
            assert(iter->is_static);
    }
#endif
}

symbol*
SymbolScope::FindGlobal(sp::Atom* atom, int fnumber) const
{
    assert(!parent_);

    auto iter = symbols_.find(atom);
    if (iter == symbols_.end())
        return nullptr;

    // In case a static shadows a global, we search statics first.
    // When reparse goes away, we can introduce a separate static scope.
    symbol* nonstatic = nullptr;
    for (auto sym = iter->second; sym; sym = sym->next) {
        if (!sym->is_static || fnumber == -1) {
            nonstatic = sym;
            continue;
        }
        if (sym->fnumber == fnumber)
            return sym;
    }
    return nonstatic;
}

void
SymbolScope::DeleteSymbols(const std::function<bool(symbol*)>& callback)
{
    auto iter = symbols_.begin();
    while (iter != symbols_.end()) {
        symbol* prev = nullptr;
        symbol* sym = iter->second;
        while (sym) {
            // |iter->second| is the list head, which we fix up if it gets
            // deleted.
            if (callback(sym)) {
                if (!prev)
                    iter->second = sym->next;
                else
                    prev->next = sym->next;
            } else {
                prev = sym;
            }
            sym = sym->next;
        }

        if (!iter->second)
            iter = symbols_.erase(iter);
        else
            iter++;
    }
}

void AddGlobal(CompileContext& cc, symbol* sym)
{
    assert(sym->vclass == sGLOBAL);

    auto scope = cc.globals();
    if (sym->is_static)
        scope->AddChain(sym);
    else
        scope->Add(sym);
}

symbol*
findconst(CompileContext& cc, SymbolScope* scope, sp::Atom* name, int fnumber)
{
    symbol* sym;

    sym = findloc(scope, name);          /* try local symbols first */
    if (sym == NULL || sym->ident != iCONSTEXPR) { /* not found, or not a constant */
        sym = findglb(cc, name, fnumber);
    }
    if (sym == NULL || sym->ident != iCONSTEXPR)
        return NULL;
    assert(sym->parent() == NULL || sym->enumfield);
    /* ^^^ constants have no hierarchy, but enumeration fields may have a parent */
    return sym;
}

symbol*
findglb(CompileContext& cc, sp::Atom* name, int fnumber)
{
    return cc.globals()->FindGlobal(name, fnumber);
}

symbol*
FindSymbol(CompileContext& cc, SymbolScope* chain, sp::Atom* name, int fnumber, SymbolScope** found)
{
    if (auto sym = findloc(chain, name, found))
        return sym;
    if (auto sym = cc.globals()->FindGlobal(name, fnumber)) {
        if (found)
            *found = cc.globals();
        return sym;
    }
    return nullptr;
}

symbol*
findloc(SymbolScope* scope, sp::Atom* name, SymbolScope** found)
{
    for (auto iter = scope; iter; iter = iter->parent()) {
        if (auto sym = iter->Find(name)) {
            if (found)
                *found = iter;
            return sym;
        }
    }
    return nullptr;
}

// Note: not idempotent
static bool
ShouldDeleteSymbol(symbol* sym, bool delete_functions)
{
    bool mustdelete;
    switch (sym->ident) {
        case iVARIABLE:
        case iARRAY:
            /* do not delete global variables if functions are preserved */
            mustdelete = delete_functions;
            break;
        case iREFERENCE:
            /* always delete references (only exist as function parameters) */
            mustdelete = true;
            break;
        case iREFARRAY:
            /* a global iREFARRAY symbol is the return value of a function: delete
             * this only if "globals" must be deleted; other iREFARRAY instances
             * (locals) are also deleted
             */
            assert(!sym->parent());
            mustdelete = true;
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
            assert(false);
            return false;
    }
    if (mustdelete)
        return true;
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
    sym->clear_refers();
    return false;
}

void
delete_symbols(CompileContext& cc, bool delete_functions)
{
    cc.globals()->DeleteSymbols([delete_functions](symbol* sym) -> bool {
        return ShouldDeleteSymbol(sym, delete_functions);
    });
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
   array(nullptr)
{
    resizeArgs(0);
}

void
FunctionData::resizeArgs(size_t nargs)
{
    args.resize(nargs);
    args.emplace_back();
}

symbol::symbol()
 : symbol(nullptr, 0, 0, 0, 0)
{}

symbol::symbol(sp::Atom* symname, cell symaddr, int symident, int symvclass, int symtag)
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
   documentation(nullptr),
   methodmap(nullptr),
   addr_(symaddr),
   name_(nullptr),
   referred_from_count_(0),
   parent_(nullptr),
   child_(nullptr)
{
    name_ = symname;
    if (symident == iFUNCTN)
        data_ = new FunctionData;
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
    documentation = other.documentation;
    // Note: explicitly don't add queued.

    x = other.x;
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

symbol*
NewVariable(sp::Atom* name, cell addr, int ident, int vclass, int tag, int dim[], int numdim,
            int semantic_tag)
{
    symbol* sym;

    if (ident == iARRAY || ident == iREFARRAY) {
        symbol *parent = NULL, *top;
        int level;
        sym = NULL; /* to avoid a compiler warning */
        for (level = 0; level < numdim; level++) {
            top = new symbol(name, addr, ident, vclass, tag);
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
        sym = new symbol(name, addr, ident, vclass, tag);
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
FindEnumStructField(Type* type, sp::Atom* name)
{
    assert(type->asEnumStruct());

    auto const_name = ke::StringPrintf("%s::%s", type->name(), name->chars());
    auto atom = gAtoms.add(const_name);
    return findglb(CompileContext::get(), atom, -1);
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
fetchfunc(CompileContext& cc, sp::Atom* name, int fnumber)
{
    symbol* sym;

    if ((sym = findglb(cc, name, fnumber)) != 0) { /* already in symbol table? */
        if (sym->ident != iFUNCTN) {
            report(21) << name; /* yes, but not as a function */
            return nullptr;     /* make sure the old symbol is not damaged */
        } else if (sym->native) {
            report(21) << name; /* yes, and it is a native */
        }
        assert(sym->vclass == sGLOBAL);
    } else {
        /* don't set the "uDEFINE" flag; it may be a prototype */
        sym = new symbol(name, code_idx, iFUNCTN, sGLOBAL, 0);
        AddGlobal(cc, sym);
        assert(sym); /* fatal error 103 must be given on error */
    }
    if (pc_deprecate.size() > 0) {
        assert(sym);
        sym->deprecated = true;
        if (sc_status == statWRITE && !pc_deprecate.empty())
            sym->documentation = new PoolString(pc_deprecate);
        pc_deprecate.clear();
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
reduce_referrers(CompileContext& cc)
{
    std::vector<symbol*> work;

    // Enqueue all unreferred symbols.
    cc.globals()->ForEachSymbol([&](symbol* sym) -> void {
        if (is_symbol_unused(sym)) {
            sym->queued = true;
            work.push_back(sym);
        }
    });

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
deduce_liveness(CompileContext& cc)
{
    std::vector<symbol*> work;

    // The root set is all public functions.
    cc.globals()->ForEachSymbol([&](symbol* sym) -> void {
        if (sym->ident != iFUNCTN)
            return;
        if (sym->native)
            return;

        if (sym->is_public) {
            sym->queued = true;
            work.push_back(sym);
        } else {
            sym->queued = false;
        }
    });

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
    cc.globals()->ForEachSymbol([&](symbol* sym) -> void {
        if (sym->ident != iFUNCTN || sym->queued)
            return;
        if (sym->native)
            return;
        sym->usage &= ~(uWRITTEN | uREAD);
    });
}

/*  add_constant
 *
 *  Adds a symbol to the symbol table. Returns NULL on failure.
 */
symbol*
add_constant(CompileContext& cc, SymbolScope* scope, sp::Atom* name, cell val, int vclass,
             int tag, int fnumber)
{
    /* Test whether a global or local symbol with the same name exists. Since
     * constants are stored in the symbols table, this also finds previously
     * defind constants. */
    SymbolScope* found;
    if (symbol* sym = FindSymbol(cc, scope, name, fnumber, &found)) {
        if (found == scope || (found == cc.globals() && vclass == sGLOBAL)) {
            report(21) << name; /* symbol already defined */
            return sym;
        }
    }

    auto sym = new symbol(name, val, iCONSTEXPR, vclass, tag);
    if (vclass == sGLOBAL)
        AddGlobal(cc, sym);
    else
        scope->Add(sym);

    sym->defined = true;
    if (sc_status == statIDLE)
        sym->predefined = true;
    return sym;
}

