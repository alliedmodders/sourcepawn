// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) AlliedModders LLC 2026
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
#include "coercion-rules.h"

#include "parse-node.h"
#include "sctracker.h"
#include "semantics-inl.h"
#include "semantics.h"

namespace sp {
namespace cc {

static bool IsReturnOrAssign(CvtContext why) {
    return why == CvtContext::Assignment || why == CvtContext::Return;
}

static ConversionKind FindBuiltinConversion(BuiltinType kind, Type* to, CvtContext why) {
    switch (kind) {
        case BuiltinType::Bool:
            if (to->isAny())
                return ConversionKind::Trivial;
            if (to->isInt() && (IsReturnOrAssign(why) || why == CvtContext::Argument))
                return ConversionKind::Trivial;
            if (to->isEnum() || to->isMethodmap() || to->isChar() || to->isInt())
                return ConversionKind::TagMismatch;
            break;

        case BuiltinType::Char:
            if (to->isAny() || to->isInt())
                return ConversionKind::Trivial;
            if (to->isBool() && (why == CvtContext::Return || why == CvtContext::Argument))
                return ConversionKind::Trivial;
            if (to->isEnum() || to->isMethodmap() || to->isBool())
                return ConversionKind::TagMismatch;
            break;

        case BuiltinType::Int:
            if (to->isAny() || to->isChar())
                return ConversionKind::Trivial;

            // Rule for legacy compatibility.
            if (IsReturnOrAssign(why) && to->isInt())
                return ConversionKind::Trivial;

            if (to->isBool() && (why == CvtContext::Return || why == CvtContext::Argument))
                return ConversionKind::Trivial;
            if (to->isBool() || to->isEnum() || to->isMethodmap())
                return ConversionKind::TagMismatch;
            if (to->isFloat() || to->isInt64())
                return ConversionKind::Numeric;
            break;

        case BuiltinType::Null:
            if (to->isNullable())
                return ConversionKind::None;
            if (auto map = to->asMethodmap(); map && map->nullable())
                return ConversionKind::CoerceNull;
            if (to->isFunction() || to->as<FunctionType>() || to->isTypedef())
                return ConversionKind::CoerceNull;
            break;

        case BuiltinType::Any:
            if (to->isInt64())
                return ConversionKind::Numeric;
            if (to->coercesFromInt() || to->isFloat() || to->isFunction() || to->as<FunctionType>())
                return ConversionKind::Trivial;
            break;

        case BuiltinType::Float:
            if (to->isAny())
                return ConversionKind::Trivial;
            break;

        default:
            break;
    }

    return ConversionKind::Illegal;
}

ConversionKind FindArrayConversion(ArrayType* from, ArrayType* to, CvtContext why) {
    if (from->rank() != to->rank())
        return ConversionKind::Illegal;

    // Assigning flat arrays requires a deep copy to a fixed-size array.
    if (why == CvtContext::Assignment && (!to->is_fixed() && from->is_flat()))
        return ConversionKind::Illegal;

    // Hacky shortcut for single rank arrays, allowing strings to be copied
    // into char arrays of greater or equal size.
    if ((IsReturnOrAssign(why) || why == CvtContext::Argument) &&
        from->isCharArray() && to->isCharArray() && from->size() && from->size() <= to->size())
    {
        return ConversionKind::Trivial;
    }

    ArrayType* from_iter = from;
    ArrayType* to_iter = to;
    while (from_iter) {
        if (from_iter->size()) {
            if (to_iter->size() && from_iter->size() != to_iter->size())
                return ConversionKind::Illegal;
        } else {
            if (to_iter->size())
                return ConversionKind::Illegal;
        }
        auto next = from_iter->inner()->as<ArrayType>();
        if (!next)
            break;
        from_iter = next;
        to_iter = to_iter->inner()->as<ArrayType>();
    }

    if (from_iter->inner() != to_iter->inner()) {
        if (why != CvtContext::Argument && why != CvtContext::FuncArg)
            return ConversionKind::Illegal;

        if (to_iter->inner()->isAny()) {
            if (from_iter->inner()->maybe_lit_size().value_or(0) != 4)
                return ConversionKind::Illegal;
            return ConversionKind::Trivial;
        }

        if (from_iter->inner()->isAny()) {
            if (to_iter->inner()->maybe_lit_size().value_or(0) != 4)
                return ConversionKind::Illegal;
            return ConversionKind::Trivial;
        }

        // For FuncArg, only any coercions are allowed (handled above).
        if (why == CvtContext::FuncArg)
            return ConversionKind::Illegal;

        auto ck = FindConversion(from_iter->inner(), to_iter->inner(), why);
        if (IsNopConversion(ck))
            return ck;
        return ConversionKind::Illegal;
    }
    return ConversionKind::Trivial;
}

static ConversionKind FindEnumConversion(Type* from, Type* to, CvtContext why) {
    if (from->isMethodmap() && to->isMethodmap()) {
        if (HasTagOnInheritanceChain(from, to))
            return ConversionKind::None;
        if (why == CvtContext::FuncArg && HasTagOnInheritanceChain(to, from))
            return ConversionKind::Trivial;
    }

    if (to->isEnum () || to->isMethodmap())
        return ConversionKind::TagMismatch;

    if (!to->isBuiltin())
        return ConversionKind::Illegal;

    switch (to->builtin_type()) {
        case BuiltinType::Bool:
        case BuiltinType::Char:
        case BuiltinType::Float:
            return ConversionKind::TagMismatch;
        case BuiltinType::Int:
            // Rule for legacy compatibility.
            if (IsReturnOrAssign(why) || why == CvtContext::Argument)
                return ConversionKind::Trivial;
            return ConversionKind::TagMismatch;
        default:
            return ConversionKind::Illegal;
    }
}

static inline bool AllowReturnTypeMismatch(FunctionType* from, FunctionType* to) {
    if (from->conv() != FunctionType::Closure &&
        to->conv() == FunctionType::Legacy &&
        to->return_type()->isVoid() && from->return_type()->isInt())
    {
        return true;
    }
    return false;
}

static ConversionKind CheckFunctions(FunctionType* from, FunctionType* to, CvtContext why) {
    if (from->variadic() != to->variadic())
        return ConversionKind::Illegal;

    if (from->return_type() != to->return_type()) {
        if (!AllowReturnTypeMismatch(from, to))
            return ConversionKind::Illegal;
    }

    if (from->nargs() != to->nargs())
        return ConversionKind::Illegal;

    ConversionKind best = ConversionKind::None;
    for (size_t i = 0; i < from->nargs(); i++) {
        QualType from_arg = from->arg_type(i);
        QualType to_arg = to->arg_type(i);
        auto ck = FindConversion(*from_arg, *to_arg, CvtContext::FuncArg);
        if (ck != ConversionKind::None && ck != ConversionKind::Trivial)
            return ConversionKind::Illegal;

        // No fuzzy matching for typed signatures, unless downgrading to a legacy type.
        if (to->conv() != FunctionType::Legacy) {
            if (ck != ConversionKind::None)
                return ConversionKind::Illegal;
        }
    }
    return best;
}

static ConversionKind FindFuncConversion(FunctionType* from, Type* to, CvtContext why) {
    if (to->isCanonicalFunction()) {
        return from->conv() == FunctionType::Legacy
               ? ConversionKind::Trivial
               : ConversionKind::FuncToLegacy;
    }

    if (auto other = to->as<FunctionType>()) {
        auto ck = CheckFunctions(from, other, why);
        if (ck != ConversionKind::Illegal) {
            if (from->conv() == FunctionType::Legacy &&
                other->conv() != FunctionType::Legacy)
            {
                return ConversionKind::LegacyToFunc;
            }
            if (from->conv() != FunctionType::Legacy &&
                other->conv() == FunctionType::Legacy)
            {
                return ConversionKind::FuncToLegacy;
            }
        }
        return ck;
    }

    if (!to->isFunction())
        return ConversionKind::Illegal;

    ConversionKind best = ConversionKind::Illegal;

    auto fe = to->toFunction();
    for (const auto& other : fe->entries) {
        auto ck = CheckFunctions(from, other, why);
        if (static_cast<uint32_t>(ck) > static_cast<uint32_t>(best))
            best = ck;
        if (best == ConversionKind::None)
            break;
    }

    if (best != ConversionKind::Illegal && from->conv() != FunctionType::Legacy)
        return ConversionKind::FuncToLegacy;

    return best;
}

static ConversionKind FindFuncConversion(funcenum_t* fe, Type* to, CvtContext why) {
    if (to->isCanonicalFunction())
        return ConversionKind::Trivial;

    if (auto to_ft = to->as<FunctionType>()) {
        // typesets must never convert to typed functions, since the union allows unsafe casts.
        if (to_ft->conv() != FunctionType::Legacy)
            return ConversionKind::Illegal;
    }

    ConversionKind best = ConversionKind::Illegal;
    for (const auto& from : fe->entries) {
        auto ck = FindConversion(from, to, why);
        if (static_cast<uint32_t>(ck) > static_cast<uint32_t>(best))
            best = ck;
        if (best == ConversionKind::None)
            break;
    }
    return best;
}

ConversionKind FindConversion(Type* from, Type* to, CvtContext why) {
    if (why == CvtContext::Argument) {
        if (from->isReference())
            from = from->inner();
        if (to->isReference())
            to = to->inner();
    }

    // Early shortcut for identity. We never check identity again.
    if (from == to)
        return ConversionKind::None;

    switch (from->kind()) {
        case TypeKind::Builtin:
            return FindBuiltinConversion(from->builtin_type(), to, why);

        case TypeKind::Object:
            assert(false);
            break;

        case TypeKind::EnumStruct:
        case TypeKind::Pstruct:
            return ConversionKind::Illegal;

        case TypeKind::Methodmap:
        case TypeKind::Enum:
            if (to->isAny())
                return ConversionKind::Trivial;
            return FindEnumConversion(from, to, why);

        case TypeKind::Function:
            if (to->isAny())
                return ConversionKind::Trivial;
            if (from->isCanonicalFunction())
                return ConversionKind::Illegal;
            return FindFuncConversion(from->toFunction(), to, why);

        case TypeKind::FunctionSignature:
            if (to->isAny())
            {
                auto from_ft = from->to<FunctionType>();
                return from_ft->conv() == FunctionType::Legacy
                       ? ConversionKind::Trivial
                       : ConversionKind::FuncToLegacy;
            }
            return FindFuncConversion(from->to<FunctionType>(), to, why);

        case TypeKind::Typedef:
            assert(false);
            break;

        case TypeKind::Reference:
            return ConversionKind::Illegal;

        case TypeKind::Array: {
            auto other = to->as<ArrayType>();
            if (!other)
                return ConversionKind::Illegal;
            return FindArrayConversion(from->to<ArrayType>(), other, why);
        }
    }

    return ConversionKind::Illegal;
}

} // namespace cc
} // namespace sp
