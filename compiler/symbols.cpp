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

namespace sp {
namespace cc {

void markusage(Decl* decl, int usage) {
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

bool check_operatortag(int opertok, Type* result_type, const char* opername) {
    assert(opername != NULL && strlen(opername) > 0);
    switch (opertok) {
        case '!':
        case '<':
        case '>':
        case tlEQ:
        case tlNE:
        case tlLE:
        case tlGE:
            if (!result_type->isBool()) {
                report(63) << opername << "bool"; /* operator X requires a "bool:" result tag */
                return false;
            }
            break;
        case '~':
            if (!result_type->isInt()) {
                report(63) << opername << "int"; /* operator "~" requires a "_:" result tag */
                return false;
            }
            break;
    }
    return true;
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

Decl* FindSymbol(SymbolScope* scope, Atom* name, SymbolScope** found) {
    for (auto iter = scope; iter; iter = iter->parent()) {
        if (auto decl = iter->Find(name)) {
            if (found)
                *found = iter;
            return decl;
        }
    }
    return nullptr;
}

Decl* FindSymbol(SemaContext& sc, Atom* name, SymbolScope** found) {
    return FindSymbol(sc.scope(), name, found);
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
