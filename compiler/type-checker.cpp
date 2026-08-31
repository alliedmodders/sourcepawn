/* vim: set sts=4 ts=8 sw=4 tw=99 et: */
//
//  Copyright (c) AlliedModders LLC, 2024
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

#include "semantics.h"

#include <amtl/am-raii.h>
#include "errors.h"
#include "sctracker.h"

namespace sp {
namespace cc {

bool Semantics::PerformTypeCheck(const token_pos_t& pos, QualType formal, QualType actual,
                                 TypeContext why, int flags)
{
    TypeCheckerState state(pos, formal, actual, why, flags);
    return CheckType(state);
}

bool Semantics::PerformTypeCheck(ParseNode* node, QualType formal, QualType actual,
                                 TypeContext why, int flags)
{
    return PerformTypeCheck(node->pos(), formal, actual, why, flags);
}

bool Semantics::PerformCoercion(const token_pos_t& pos, QualType formal, QualType actual,
                                TypeContext why, int flags)
{
    return PerformTypeCheck(pos, formal, actual, why, flags | AllowCoerce);
}

bool Semantics::PerformCoercion(ParseNode* node, QualType formal, QualType actual,
                                TypeContext why, int flags)
{
    return PerformCoercion(node->pos(), formal, actual, why, flags);
}

bool Semantics::CheckType(TypeCheckerState& state) {
    auto report = ke::MakeScopeGuard([&state]() -> void {
        state.defer.Report();
    });

    if (CheckTypeImpl(state))
        return true;

    if (!(state.flags & Commutative))
        return false;

    AutoDeferReports defer_again(CompileContext::get());

    std::swap(state.formal, state.actual);
    if (!CheckTypeImpl(state))
        return false;

    report.cancel();
    defer_again.Report();
    return true;
}

bool Semantics::DiagnoseFailure(TypeCheckerState& state) {
    if (!state.defer.HasErrors())
        report(state.pos, 450) << state.actual << state.formal;
    return false;
}

bool Semantics::DiagnoseFunctionFailure(TypeCheckerState& state) {
    if (!state.defer.HasErrors())
        report(state.pos, 100);
    return DiagnoseFailure(state);
}

bool Semantics::CheckTypeImpl(TypeCheckerState& state) {
    if (auto formal_array = state.formal->as<ArrayType>()) {
        if (state.actual->isNull()) {
            if (!formal_array->is_fixed())
                return true;
        }
        return CheckArrays(state, formal_array, state.actual->as<ArrayType>());
    }

    Type* formal = *state.formal;
    Type* actual = *state.actual;
    if (state.flags & AllowCoerce) {
        if (formal->isReference())
            formal = formal->inner();
        if (actual->isReference())
            actual = actual->inner();
    }

    return CheckValueType(state, formal, actual);
}

bool Semantics::CheckValueType(TypeCheckerState& state, Type* formal, Type* actual) {
    if (formal->isEnumStruct()) {
        if (formal != actual)
            return DiagnoseFailure(state);
        return true;
    }

    if (formal == actual)
        return true;

    if (formal->isObject()) {
        // No object types yet.
        return DiagnoseFailure(state);
    }

    if (actual->isNull()) {
        if (formal->isNullable())
            return true;
        if (auto map = formal->asMethodmap()) {
            if (map->nullable())
                return true;
        }
        if (formal->isFunction())
            return true;

        report(state.pos, 148) << state.formal;
        return DiagnoseFailure(state);
    }

    if ((formal->isInt64() || actual->isInt64()) && (formal != actual)) {
        report(state.pos, 450) << actual << formal;
        return DiagnoseFailure(state);
    }

    if (state.flags & AllowCoerce) {
        if ((formal->isInt() || formal->isAny()) && actual->coercesFromInt())
            return true;
        if (formal->isBool() && (actual->isInt() || actual->isChar()))
            return true;
        if (actual->isAny() && (formal->coercesFromInt() || formal->isFloat()))
            return true;
    }

    // We allow this even on function signature checks as a convenient shorthand,
    // even though it violates standard contravariance rules.
    if (formal->isAny()) {
        if (actual->isVoid())
            return DiagnoseFailure(state);
        return true;
    }

    if (formal->isFunction())
        return CheckFunction(state);

    if (state.flags & (AllowCoerce | FuncArg)) {
        // See if the type has a methodmap associated with it. If so, see if the given
        // type is anywhere on the inheritance chain.
        if (HasTagOnInheritanceChain(actual, formal))
            return true;
    }

    if (state.flags & FuncArg) {
        // As a special exception to the "any" rule above, we allow the inverse
        // to succeed for signature matching. This is a convenience and allows
        // something like:
        //
        //   void f(DataPack x);
        //   void g(void f(Handle h), Handle h) {
        //     f(h);
        //   }
        //
        // In the future, we can insert a runtime check here. For now, we can't,
        // but we allow it anyway.
        if (HasTagOnInheritanceChain(formal, actual))
            return true;
    }

    if ((formal->isEnum() || formal->isMethodmap()) && actual->isInt()) {
        if (state.flags & EnumAssign)
            return true;

        report(state.pos, 253) << actual << formal;
        return true;
    }

    if (state.flags & AllowCoerce) {
        if (formal->isChar() && actual->isInt())
            return true;

        // Get rid of this long-term.
        if (formal->isFloat() && actual->isInt()) {
            report(state.pos, 253) << state.actual << state.formal;
            return true;
        }
    }

    return DiagnoseFailure(state);
}

bool Semantics::CheckArrays(TypeCheckerState& state, ArrayType* formal, ArrayType* actual) {
    if (state.why != Argument && state.why != Return &&
        !formal->is_fixed() && actual->is_flat())
    {
        if (!state.defer.HasErrors())
            report(state.pos, 473) << state.actual << state.formal;
        return false;
    }

    // When enum structs can contain nested references, we will have to forbid
    // coercion to |any|. Or, more likely, create a proper struct type.
    if (!actual) {
        // Arguments allow implicit array slices and coercion from enum structs
        // to any[].
        if (state.why != Argument || !(state.flags & AllowCoerce))
            return DiagnoseFailure(state);
        if (formal->inner()->isArray())
            return DiagnoseFailure(state);
        if (state.actual->asEnumStruct()) {
            if (!formal->inner()->isAny())
                return DiagnoseFailure(state);
            return true;
        }
        return DiagnoseFailure(state);
    }

    for (;;) {
        if (formal->size()) {
            if ((state.flags & (AllowCoerce | Ternary)) &&
                actual->isCharArray())
            {
                if (!(state.flags & Ternary) && formal->size() < actual->size())
                    return DiagnoseFailure(state);
            } else {
                if (formal->size() != actual->size())
                    return DiagnoseFailure(state);
            }
        }
        auto next_formal = formal->inner()->as<ArrayType>();
        if (!next_formal)
            break;
        auto next_actual = actual->inner()->as<ArrayType>();
        if (!next_actual)
            return DiagnoseFailure(state);
        formal = next_formal;
        actual = next_actual;
    }

    auto formal_elt = formal->inner();
    auto actual_elt = actual->inner();
    if (formal_elt == actual_elt)
        return true;

    if ((state.flags & FuncArg) &&
        ((formal_elt->isAny() && actual_elt->hasCellSize()) ||
         (actual_elt->isAny() && formal_elt->hasCellSize())))
    {
        return true;
    }

    if (state.why == Argument && (state.flags & AllowCoerce)) {
        if ((actual_elt->isEnum() || actual_elt->isMethodmap()) && formal_elt->isInt()) {
            report(state.pos, 253) << state.actual << state.formal;
            return true;
        }
        if (formal_elt->isAny() && actual_elt->hasCellSize())
            return true;
        if (actual_elt->isAny() && formal_elt->hasCellSize())
            return true;
    }

    return DiagnoseFailure(state);
}

bool Semantics::CheckFunction(TypeCheckerState& state) {
    if (state.formal->isCanonicalFunction() && state.actual->isFunction())
        return true;

    if (state.actual->isNull())
        return true;

    if (!state.actual->isFunction())
        return DiagnoseFailure(state);

    auto actual_fe = state.actual->asFunction();
    if (!actual_fe || actual_fe->entries.empty())
        return DiagnoseFailure(state);

    FunctionType* actualfn = actual_fe->entries.back();
    if (!actualfn)
        return DiagnoseFailure(state);

    funcenum_t* e = state.formal->toFunction();
    if (!e)
        return DiagnoseFailure(state);

    for (const auto& formalfn : e->entries) {
        AutoDeferReports defer(CompileContext::get());
        if (CheckFunctionSignature(state, formalfn, actualfn))
            return true;
    }
    return DiagnoseFunctionFailure(state);
}

bool Semantics::CheckFunctionSignature(TypeCheckerState& state, FunctionType* formal, FunctionType* actual) {
    if (formal->return_type() != actual->return_type()) {
        if (formal->return_type()->isVoid() && actual->return_type()->isInt())
            return true;
        return DiagnoseFunctionFailure(state);
    }

    if (formal->variadic() != actual->variadic())
        return DiagnoseFunctionFailure(state);

    // Make sure there are no trailing arguments.
    if (actual->nargs() > formal->nargs())
        return DiagnoseFunctionFailure(state);

    // Check arguments.
    for (size_t i = 0; i < formal->nargs(); i++) {
        if (i >= actual->nargs())
            return DiagnoseFunctionFailure(state);

        auto formal_type = formal->arg_type(i);
        auto actual_type = actual->arg_type(i);
        if (!PerformTypeCheck(state.pos, formal_type, actual_type, Generic,
                              FuncArg))
        {
            return DiagnoseFunctionFailure(state);
        }
    }
    return true;
}

} // namespace cc
} // namespace sp
