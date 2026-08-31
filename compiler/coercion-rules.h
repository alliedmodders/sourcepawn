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
