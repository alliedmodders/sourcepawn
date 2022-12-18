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
#include "semantics.h"

void
SymbolScope::Add(symbol* sym)
{
    if (!symbols_) {
        auto& cc = CompileContext::get();
        symbols_ = cc.NewSymbolMap();
    }

    assert(symbols_->find(sym->nameAtom()) == symbols_->end());
    symbols_->emplace(sym->nameAtom(), sym);
}

void
SymbolScope::AddChain(symbol* sym)
{
    if (!symbols_) {
        auto& cc = CompileContext::get();
        symbols_ = cc.NewSymbolMap();
    }

    auto iter = symbols_->find(sym->nameAtom());
    if (iter == symbols_->end()) {
        symbols_->emplace(sym->nameAtom(), sym);
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

    auto& cc = CompileContext::get();
    if (!cc.sema())
        return;

    auto parent_func = cc.sema()->func();
    if (!parent_func)
        return;

    // The reference graph only contains outgoing edges to global or file-static
    // variables. Locals and such are computed by TestSymbols and don't need
    // special handling, there's no concept of "stock" there.
    if (sym->vclass != sGLOBAL && sym->vclass != sSTATIC)
        return;
    if (sym->ident != iFUNCTN)
        return;

    parent_func->add_reference_to(sym);
}

FunctionData::FunctionData()
  : node(nullptr),
    forward(nullptr),
    alias(nullptr),
    checked_one_signature(false),
    compared_prototype_args(false),
    is_member_function(false)
{
}

symbol::symbol(sp::Atom* symname, cell symaddr, IdentifierKind symident, int symvclass, int symtag)
 : next(nullptr),
   codeaddr(0),
   vclass((char)symvclass),
   tag(symtag),
   ident(symident),
   usage(0),
   defined(false),
   is_const(false),
   stock(false),
   is_public(false),
   is_static(false),
   is_struct(false),
   callback(false),
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
   semantic_tag(0),
   dim_data(nullptr),
   fnumber(0),
   /* assume global visibility (ignored for local symbols) */
   lnumber(0),
   documentation(nullptr),
   addr_(symaddr),
   name_(nullptr)
{
    assert(ident != iINVALID);
    name_ = symname;
    if (symident == iFUNCTN)
        data_ = new FunctionData;
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
    semantic_tag = other.semantic_tag;
    // Note: explicitly don't add queued.

    if (other.dim_data) {
        set_dim_count(other.dim_count());
        for (int i = 0; i < other.dim_count(); i++)
            set_dim(i, other.dim(i));
    }
}

void symbol::set_dim_count(int dim_count) {
    if (this->dim_count() == dim_count)
        return;

    auto& cc = CompileContext::get();
    dim_data = cc.allocator().alloc<int>(dim_count + 1);
    dim_data[0] = dim_count;
    dim_data++;
}

void
symbol::add_reference_to(symbol* other)
{
    for (symbol* sym : function()->refers_to) {
        if (sym == other)
            return;
    }
    function()->refers_to.emplace_front(other);
}

bool
symbol::must_return_value() const
{
    auto types = CompileContext::get().types();
    assert(ident == iFUNCTN);
    return retvalue_used || (explicit_return_type && tag != types->tag_void());
}

bool
symbol::is_variadic() const
{
    assert(ident == iFUNCTN);
    const auto& args = function()->node->args();
    return !args.empty() && args.back()->type().ident == iVARARGS;
}

symbol*
NewVariable(sp::Atom* name, cell addr, IdentifierKind ident, int vclass, int tag, int dim[],
            int numdim, int semantic_tag)
{
    symbol* sym = new symbol(name, addr, ident, vclass, tag);

    if (numdim) {
        sym->set_dim_count(numdim);
        for (int i = 0; i < numdim; i++)
            sym->set_dim(i, dim[i]);
        sym->semantic_tag = semantic_tag;
    }
    return sym;
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
            if (resulttag != CompileContext::get().types()->tag_bool()) {
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

// Determine the set of live functions.
void
deduce_liveness(CompileContext& cc)
{
    std::vector<symbol*> work;

    // The root set is all public functions.
    for (const auto& sym : cc.publics()) {
        if (sym->native)
            return;

        if (sym->is_public) {
            sym->queued = true;
            sym->usage |= uLIVE;
            work.push_back(sym);
        } else {
            sym->queued = false;
        }
    }

    auto enqueue = [&](symbol* other) -> bool {
        if (other->ident != iFUNCTN || other->queued)
            return false;
        other->queued = true;
        other->usage |= uLIVE;
        work.push_back(other);
        return true;
    };

    // Traverse referrers to find the transitive set of live functions.
    while (!work.empty()) {
        symbol* live = ke::PopBack(&work);

        for (const auto& other : live->function()->refers_to) {
            if (!enqueue(other))
                continue;
            if (auto alias = other->function()->alias)
                enqueue(alias);
        }
    }
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
    symbol* sym = nullptr;
    if (sc.func_node() && sc.func_node()->is_native()) {
        sym = sc.scope()->Find(name);
        scope = sc.scope();
    } else {
        sym = FindSymbol(sc, name, &scope);
    }
    if (!sym)
        return NewNameStatus::Ok;

    SymbolScope* current = sc.ScopeForAdd();
    if (scope->kind() == sGLOBAL && current->IsGlobalOrFileStatic()) {
        if (vclass == sSTATIC)
            return NewNameStatus::Shadowed;
        return NewNameStatus::Duplicated;
    }
    if (scope == current)
        return NewNameStatus::Duplicated;
    if (current->kind() == sARGUMENT && sym->ident == iFUNCTN)
        return NewNameStatus::Ok;
    return NewNameStatus::Shadowed;
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
    auto scope = sc.ScopeForAdd();
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
