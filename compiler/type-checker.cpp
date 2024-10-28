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
#include "type-checker.h"

#include <amtl/am-raii.h>
#include "errors.h"
#include "sctracker.h"

namespace sp {
namespace cc {

TypeChecker::TypeChecker(const token_pos_t& pos, QualType formal, QualType actual, Context why,
                         int flags)
  : pos_(pos),
    formal_(formal),
    actual_(actual),
    why_(why),
    flags_(flags),
    defer_(CompileContext::get())
{}

bool TypeChecker::Coerce() {
    flags_ = AllowCoerce;
    return Check();
}

bool TypeChecker::Check() {
    auto report = ke::MakeScopeGuard([this]() -> void {
        defer_.Report();
    });

    if (CheckImpl())
        return true;

    if (!(flags_ & Commutative))
        return false;

    AutoDeferReports defer_again(CompileContext::get());

    std::swap(formal_, actual_);
    if (!CheckImpl())
        return false;

    report.cancel();
    defer_again.Report();
    return true;
}

bool TypeChecker::DiagnoseFailure() {
    if (!defer_.HasErrors())
        report(pos_, 450) << actual_ << formal_;
    return false;
}

bool TypeChecker::DiagnoseFunctionFailure() {
    if (!defer_.HasErrors())
        report(pos_, 100);
    return DiagnoseFailure();
}

bool TypeChecker::CheckImpl() {
    if (auto formal_array = formal_->as<ArrayType>())
        return CheckArrays(formal_array, actual_->as<ArrayType>());

    Type* formal = *formal_;
    Type* actual = *actual_;
    if (flags_ & AllowCoerce) {
        if (formal->isReference())
            formal = formal->inner();
        if (actual->isReference())
            actual = actual->inner();
    }

    return CheckValueType(formal, actual);
}

bool TypeChecker::CheckValueType(Type* formal, Type* actual) {
    if (formal->isEnumStruct()) {
        if (formal != actual)
            return DiagnoseFailure();
        return true;
    }

    if (formal == actual)
        return true;

    if (formal->isObject()) {
        // No object types yet.
        return DiagnoseFailure();
    }

    if (actual->isNull()) {
        if (auto map = formal->asMethodmap()) {
            if (map->nullable())
                return true;
        }
        if (formal->isFunction())
            return true;

        report(pos_, 148) << formal_;
        return DiagnoseFailure();
    }

    if (flags_ & AllowCoerce) {
        if ((formal->isInt() || formal->isAny()) && actual->coercesFromInt())
            return true;
        if (formal->isBool() && (actual->isInt() || actual->isChar()))
            return true;
        if (actual->isAny() && (formal->coercesFromInt() || formal->isFloat()))
            return true;
    }

    // We allow this even on function signature checks as a convenient shorthand,
    // even though it violates standard contravariance rules.
    if (formal->isAny())
        return true;

    if (formal->isFunction())
        return CheckFunction();

    if (flags_ & (AllowCoerce | FuncArg)) {
        // See if the type has a methodmap associated with it. If so, see if the given
        // type is anywhere on the inheritance chain.
        if (HasTagOnInheritanceChain(actual, formal))
            return true;
    }

    if (flags_ & FuncArg) {
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
        if (flags_ & EnumAssign)
            return true;

        report(pos_, 253) << actual << formal;
        return true;
    }

    if (flags_ & AllowCoerce) {
        if (formal->isChar() && actual->isInt())
            return true;

        // Get rid of this long-term.
        if (formal->isFloat() && actual->isInt()) {
            report(pos_, 253) << actual_ << formal_;
            return true;
        }
    }

    return DiagnoseFailure();
}

bool TypeChecker::CheckArrays(ArrayType* formal, ArrayType* actual) {
    // When enum structs can contain nested references, we will have to forbid
    // coercion to |any|. Or, more likely, create a proper struct type.
    if (!actual) {
        // Arguments allow implicit array slices and coercion from enum structs
        // to any[].
        if (why_ != Argument || !(flags_ & AllowCoerce))
            return DiagnoseFailure();
        if (formal->inner()->isArray())
            return DiagnoseFailure();
        if (auto es = actual_->asEnumStruct()) {
            if (formal->size() && formal->size() != es->array_size())
                return DiagnoseFailure();
            if (!formal->inner()->isAny())
                return DiagnoseFailure();
            return true;
        }
        return DiagnoseFailure();
    }

    for (;;) {
        if (formal->size()) {
            if ((flags_ & (AllowCoerce | Ternary)) && actual->isCharArray()) {
                if (!(flags_ & Ternary) && formal->size() < actual->size())
                    return DiagnoseFailure();
            } else {
                if (formal->size() != actual->size())
                    return DiagnoseFailure();
            }
        }
        auto next_formal = formal->inner()->as<ArrayType>();
        if (!next_formal)
            break;
        auto next_actual = actual->inner()->as<ArrayType>();
        if (!next_actual)
            return DiagnoseFailure();
        formal = next_formal;
        actual = next_actual;
    }

    auto formal_elt = formal->inner();
    auto actual_elt = actual->inner();
    if (formal_elt == actual_elt)
        return true;

    if ((flags_ & FuncArg) &&
        ((formal_elt->isAny() && actual_elt->hasCellSize()) ||
         (actual_elt->isAny() && formal_elt->hasCellSize())))
    {
        return true;
    }

    if (why_ == Argument && (flags_ & AllowCoerce)) {
        if ((actual_elt->isEnum() || actual_elt->isMethodmap()) && formal_elt->isInt()) {
            report(pos_, 253) << actual_ << formal_;
            return true;
        }
        if (formal_elt->isAny() && actual_elt->hasCellSize())
            return true;
        if (actual_elt->isAny() && formal_elt->hasCellSize())
            return true;
    }

    return DiagnoseFailure();
}

bool TypeChecker::CheckFunction() {
    if (formal_->isCanonicalFunction() && actual_->isFunction())
        return true;

    if (actual_->isNull())
        return true;

    if (!actual_->isFunction())
        return DiagnoseFailure();

    auto actual_fe = actual_->asFunction();
    if (!actual_fe || actual_fe->entries.empty())
        return DiagnoseFailure();

    FunctionType* actualfn = actual_fe->entries.back();
    if (!actualfn)
        return DiagnoseFailure();

    funcenum_t* e = formal_->toFunction();
    if (!e)
        return DiagnoseFailure();

    for (const auto& formalfn : e->entries) {
        AutoDeferReports defer(CompileContext::get());
        if (CheckFunctionSignature(formalfn, actualfn))
            return true;
    }
    return DiagnoseFunctionFailure();
}

bool TypeChecker::CheckFunctionSignature(FunctionType* formal, FunctionType* actual) {
    if (formal->return_type() != actual->return_type()) {
        if (formal->return_type()->isVoid() && actual->return_type()->isInt())
            return true;
        return DiagnoseFunctionFailure();
    }

    if (formal->variadic() != actual->variadic())
        return DiagnoseFunctionFailure();

    // Make sure there are no trailing arguments.
    if (actual->nargs() > formal->nargs())
        return DiagnoseFunctionFailure();

    // Check arguments.
    for (size_t i = 0; i < formal->nargs(); i++) {
        if (i >= actual->nargs())
            return DiagnoseFunctionFailure();

        auto formal_type = formal->arg_type(i);
        auto actual_type = actual->arg_type(i);
        TypeChecker tc(pos_, formal_type, actual_type, TypeChecker::Generic,
                       TypeChecker::FuncArg);
        if (!tc.Check())
            return DiagnoseFunctionFailure();
    }
    return true;
}

bool TypeChecker::DoCoerce(Type* formal, Expr* actual) {
    TypeChecker tc(actual, formal, actual->val().type(), Generic);
    return tc.Coerce();
}

} // namespace cc
} // namespace sp
