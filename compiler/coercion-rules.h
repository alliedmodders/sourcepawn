// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

#include <stdint.h>

#include <optional>

#include "types.h"

namespace sp {
namespace cc {

// These are ordered from "worst" to "best" conversion scenarios.
enum class ConversionKind : uint32_t {
    // No conversion is possible.
    Illegal,
    // Conversion is possible with an explicit cast.
    NeedsCast,
    // Convert SpFunction to legacy funcid_t (OP_GETFUNCID).
    FuncToLegacy,
    // Convert legacy funcid_t to SpFunction (OP_GETFNOBJ).
    LegacyToFunc,
    // Trivial conversion that would yield a tag warning in the tag system.
    TagMismatch,
    // A numeric conversion operation is needed.
    Numeric,
    // Coercion of null to an integer 0 for nullable methodmaps/functions.
    CoerceNull,
    // Trivial conversion that is internally a bitcast.
    Trivial,
    // No conversion is necessary.
    None,
};

enum class CvtContext {
    Argument,
    Assignment,
    Explicit,
    Operator,
    FuncArg,
    Return,
};

static inline bool HasImplicitConversion(ConversionKind ck) {
    return static_cast<uint32_t>(ck) > static_cast<uint32_t>(ConversionKind::NeedsCast);
}

static inline bool IsNopConversion(ConversionKind ck) {
    switch (ck) {
        case ConversionKind::TagMismatch:
        case ConversionKind::Trivial:
        case ConversionKind::None:
            return true;
        default:
            return false;
    }
}

struct Conversion {
    ConversionKind ck = ConversionKind::Illegal;
    Type* type = nullptr;

    bool IsImplicit() const { return HasImplicitConversion(ck); }
    bool IsNop() const { return IsNopConversion(ck); }
};

ConversionKind FindConversion(Type* from, Type* to, CvtContext why);

} // namespace cc
} // namespace sp
