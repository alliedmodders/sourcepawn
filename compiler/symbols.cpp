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
#include "scopes.h"
#include "semantics.h"

namespace sp {
namespace cc {

void markusage(Decl* decl, int usage) {
    if (auto upvar = decl->as<UpvarDecl>()) {
        markusage(upvar->var(), usage);
        return;
    }

    if (auto var = decl->as<VarDeclBase>()) {
        if (usage & uREAD)
            var->set_is_read();
        if (usage & uWRITTEN)
            var->set_is_written();
        return;
    }

    auto& cc = CompileContext::get();
    if (!cc.sema())
        return;

    auto parent_func = cc.sema()->func();
    if (!parent_func)
        return;

    auto fun = decl->as<FunctionDecl>();
    if (!fun)
        return;
    fun = fun->canonical();

    // The reference graph only contains outgoing edges to global or file-static
    // variables. Locals and such are computed by TestSymbols and don't need
    // special handling, there's no concept of "stock" there.
    if (fun->vclass() != sGLOBAL && fun->vclass() != sSTATIC)
        return;

    assert(parent_func->canonical() == parent_func);
    parent_func->AddReferenceTo(decl->as<FunctionDecl>()->canonical());
}

void markusage(const ExprVal& val, int usage) {
    if (val.ident == iVARIABLE) {
        markusage(val.sym(), usage);
    } else if (val.ident == iUPVAR) {
        markusage(val.upvar(), usage);
    } else if (val.ident == iACCESSOR) {
        if (val.accessor()->getter())
            markusage(val.accessor()->getter(), uREAD);
        if ((usage & uWRITTEN) && val.accessor()->setter())
            markusage(val.accessor()->setter(), uREAD);
    } else if (val.ident == iFUNCTN) {
        markusage(val.fun(), usage);
    }
}

Decl* FindEnumStructField(Type* type, Atom* name) {
    auto decl = type->asEnumStruct();
    if (!decl)
        return nullptr;

    for (const auto& field : decl->fields()) {
        if (field->name() == name)
            return field;
    }
    for (const auto& method : decl->methods()) {
        if (method->decl_name() == name)
            return method;
    }
    return nullptr;
}

Decl* FindClassField(Type* type, Atom* name) {
    auto decl = type->asClass();
    if (!decl)
        return nullptr;

    for (const auto& field : decl->fields()) {
        if (field->name() == name)
            return field;
    }
    for (const auto& prop : decl->properties()) {
        if (prop->name() == name)
            return prop;
    }
    for (const auto& method : decl->methods()) {
        if (method->decl_name() == name)
            return method;
    }
    return nullptr;
}

enum class NewNameStatus {
    Ok,
    Shadowed,
    Duplicated
};

static NewNameStatus GetNewNameStatus(SemaContext& sc, Atom* name, int vclass) {
    SymbolScope* scope;
    Decl* decl = nullptr;
    if (sc.func() && sc.func()->is_native()) {
        decl = sc.scope()->Find(name);
        scope = sc.scope();
    } else {
        decl = FindSymbol(sc, name, &scope);
    }
    if (!decl)
        return NewNameStatus::Ok;

    SymbolScope* current = sc.ScopeForAdd();
    if (scope->kind() == sGLOBAL && current->IsGlobalOrFileStatic()) {
        if (vclass == sSTATIC)
            return NewNameStatus::Shadowed;
        return NewNameStatus::Duplicated;
    }
    if (scope == current)
        return NewNameStatus::Duplicated;
    if (current->kind() == sARGUMENT && decl->as<FunctionDecl>())
        return NewNameStatus::Ok;
    return NewNameStatus::Shadowed;
}

bool
CheckNameRedefinition(SemaContext& sc, Atom* name, const token_pos_t& pos, int vclass)
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

bool CheckTypeNameRedefinition(SemaContext& sc, Atom* name, const token_pos_t& pos) {
    if (!ResolveType(sc, name))
        return true;
    report(pos, 432) << name;
    return false;
}

static inline bool IsUpvar(Decl* decl) {
    switch (decl->kind()) {
        case StmtKind::VarDecl:
        case StmtKind::ArgDecl:
            return true;
        default:
            return false;
    }
}

static inline Type* FindType(SymbolScope* scope, Atom* name, int flags) {
    auto type = scope->FindType(name);
    if (!type)
        return nullptr;
    if ((flags & kResolveIdent) && !type->decl())
        return nullptr;
    return type;
}

static bool ResolveInScope(SymbolScope* scope, FunctionDecl* enclosure, Atom* name,
                           ResolvedSymbol* rs, int flags)
{
    // Identifiers are preferred over types to match the semantics of the old
    // algorithm where types existed above globals.
    if (flags & kResolveIdent) {
        if (auto decl = scope->Find(name)) {
            if (enclosure) {
                if (!IsUpvar(decl))
                    return false;
                rs->enclosure = enclosure;
            }
            rs->decl = decl;
            rs->scope = scope;
            return true;
        }
        if (!(flags & kResolveType))
            return false;
    }

    if (flags & kResolveType) {
        if (auto type = FindType(scope, name, flags)) {
            rs->scope = scope;
            rs->type = type;
            rs->decl = type->decl();
            return true;
        }
    }

    return false;
}

bool ResolveSymbol(SemaContext* sc, SymbolScope* scope, Atom* name, ResolvedSymbol* rs, int flags)
{
    SymbolScope* global = nullptr;

    SymbolScope* iter = scope;
    while (iter && !iter->IsGlobalOrFileStatic()) {
        if (ResolveInScope(iter, nullptr, name, rs, flags))
            return true;
        iter = iter->parent();
    }

    // Save the global scope, we'll come back to it later.
    global = iter;

    auto sc_iter = sc ? sc->outer() : nullptr;
    while (sc_iter && sc_iter->func()) {
        // Search enclosing scopes.
        auto scope_iter = sc_iter->scope();
        while (scope_iter && !scope_iter->IsGlobalOrFileStatic()) {
            if (ResolveInScope(scope_iter, sc_iter->func(), name, rs, flags))
                return true;
            scope_iter = scope_iter->parent();
        }
        sc_iter = sc_iter->outer();
    }

    for (auto iter = global; iter; iter = iter->parent()) {
        if (ResolveInScope(iter, nullptr, name, rs, flags))
            return true;
    }
    return false;
}

Decl* FindSymbol(SymbolScope* scope, Atom* name, SymbolScope** found) {
    ResolvedSymbol rs;
    if (!ResolveSymbol(nullptr, scope, name, &rs, kResolveIdent))
        return nullptr;
    if (found)
        *found = rs.scope;
    assert(!rs.enclosure);
    return rs.decl;
}

Decl* FindSymbol(SemaContext& sc, Atom* name, SymbolScope** found) {
    return FindSymbol(sc.scope(), name, found);
}

Type* ResolveType(SemaContext& sc, Atom* name) {
    ResolvedSymbol rs;
    if (!ResolveSymbol(&sc, sc.scope(), name, &rs, kResolveType))
        return CompileContext::get().types()->findBuiltin(name);
    return rs.type;
}

void AddScopedType(SemaContext& sc, Type* type) {
    auto scope = sc.ScopeForAdd();
    if (scope->kind() == sFILE_STATIC) {
        assert(scope->parent()->kind() == sGLOBAL);
        scope = scope->parent();
    }
    scope->AddType(type->declName(), type);
}

void DefineSymbol(SemaContext& sc, Decl* decl, int vclass) {
    auto scope = sc.ScopeForAdd();
    if (scope->kind() == sFILE_STATIC && vclass != sSTATIC) {
        // The default scope is global scope, but "file static" scope comes
        // earlier in the lookup hierarchy, so skip past it if we need to.
        assert(vclass == sGLOBAL);
        assert(scope->parent()->kind() == sGLOBAL);
        scope = scope->parent();
    }
    if (scope->kind() == sGLOBAL || scope->kind() == sFILE_STATIC)
        scope->AddChain(decl);
    else
        scope->Add(decl);
}

} // namespace cc
} // namespace sp
