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
}

void AddGlobal(CompileContext& cc, symbol* sym)
{
    assert(sym->vclass == sGLOBAL);

    auto scope = cc.globals();
    scope->AddChain(sym);
}

void
markusage(symbol* sym, int usage)
{
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
   array(nullptr),
   node(nullptr),
   forward(nullptr),
   alias(nullptr)
{
    // Always have one empty argument at the end.
    args.emplace_back();
}

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
   callback(false),
   skipped(false),
   native(false),
   returns_value(false),
   always_returns(false),
   retvalue_used(false),
   is_operator(false),
   enumroot(false),
   enumfield(false),
   deprecated(false),
   queued(false),
   explicit_return_type(false),
   x({}),
   fnumber(0),
   /* assume global visibility (ignored for local symbols) */
   lnumber(0),
   documentation(nullptr),
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
    enumroot = other.enumroot;
    enumfield = other.enumfield;
    callback = other.callback;
    skipped = other.skipped;
    returns_value = other.returns_value;
    always_returns = other.always_returns;
    is_operator = other.is_operator;
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
    return retvalue_used || (explicit_return_type && tag != pc_tag_void);
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
    symbol* sym = type->asEnumStruct();
    if (!sym->data())
        return nullptr;

    auto es = sym->data()->asEnumStruct();
    for (const auto& field : es->fields) {
        if (field->nameAtom() == name)
            return field;
    }
    for (const auto& method : es->methods) {
        if (method->nameAtom() == name)
            return method;
    }
    return nullptr;
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

// Determine the set of live functions.
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

enum class NewNameStatus {
    Ok,
    Shadowed,
    Duplicated
};

static NewNameStatus
GetNewNameStatus(SemaContext& sc, sp::Atom* name, int vclass)
{
    SymbolScope* scope;
    symbol* sym = FindSymbol(sc, name, &scope);
    if (!sym)
        return NewNameStatus::Ok;
    if (scope->kind() == sGLOBAL && sc.scope()->IsGlobalOrFileStatic()) {
        if (vclass == sSTATIC)
            return NewNameStatus::Shadowed;
        return NewNameStatus::Duplicated;
    }
    if (scope == sc.scope())
        return NewNameStatus::Duplicated;
    if (scope->IsLocalOrArgument())
        return NewNameStatus::Shadowed;
    return NewNameStatus::Ok;
}

bool
CheckNameRedefinition(SemaContext& sc, sp::Atom* name, const token_pos_t& pos, int vclass)
{
    auto name_status = GetNewNameStatus(sc, name, vclass);
    if (name_status == NewNameStatus::Duplicated) {
        report(pos, 21) << name;
        return false;
    }
    if (name_status == NewNameStatus::Shadowed)
        report(pos, 219) << name;
    return true;
}

static symbol*
NewConstant(sp::Atom* name, const token_pos_t& pos, cell val, int vclass, int tag)
{
    auto sym = new symbol(name, val, iCONSTEXPR, vclass, tag);
    sym->fnumber = pos.file;
    sym->lnumber = pos.line;
    sym->defined = true;
    return sym;
}

symbol*
DefineConstant(CompileContext& cc, sp::Atom* name, cell val, int tag)
{
    auto globals = cc.globals();
    if (auto sym = globals->Find(name)) {
        sym->setAddr(val);
        sym->tag = tag;
        return sym;
    }

    auto sym = NewConstant(name, {}, val, sGLOBAL, tag);
    globals->Add(sym);
    return sym;
}

symbol*
DefineConstant(SemaContext& sc, sp::Atom* name, const token_pos_t& pos, cell val, int vclass,
               int tag)
{
    auto sym = NewConstant(name, pos, val, vclass, tag);
    if (CheckNameRedefinition(sc, name, pos, vclass))
        DefineSymbol(sc, sym);
    return sym;
}

symbol*
FindSymbol(SymbolScope* scope, sp::Atom* name, SymbolScope** found)
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

symbol*
FindSymbol(SemaContext& sc, sp::Atom* name, SymbolScope** found)
{
    return FindSymbol(sc.scope(), name, found);
}

symbol*
declare_methodmap_symbol(CompileContext& cc, methodmap_t* map)
{
    symbol* sym = FindSymbol(cc.globals(), map->name);
    if (sym && sym->ident != iMETHODMAP) {
        if (sym->ident == iCONSTEXPR) {
            // We should only hit this on the first pass. Assert really hard that
            // we're about to kill an enum definition and not something random.
            assert(sym->ident == iCONSTEXPR);
            assert(map->tag == sym->tag);

            sym->ident = iMETHODMAP;

            // Kill previous enumstruct properties, if any.
            auto data = sym->data() ? sym->data()->asEnum() : nullptr;
            map->enum_data = data;
            sym->set_data(map);
            return sym;
        }
        report(11) << map->name;
        return nullptr;
    }

    sym = new symbol(map->name, 0, iMETHODMAP, sGLOBAL, map->tag);
    cc.globals()->Add(sym);

    sym->defined = true;
    sym->set_data(map);
    return sym;
}

void
DefineSymbol(SemaContext& sc, symbol* sym)
{
    auto scope = sc.scope();
    if (scope->kind() == sFILE_STATIC && sym->vclass != sSTATIC) {
        // The default scope is global scope, but "file static" scope comes
        // earlier in the lookup hierarchy, so skip past it if we need to.
        assert(sym->vclass == sGLOBAL);
        assert(scope->parent()->kind() == sGLOBAL);
        scope = scope->parent();
    }
    if (scope->kind() == sGLOBAL || scope->kind() == sFILE_STATIC)
        scope->AddChain(sym);
    else
        scope->Add(sym);
}
