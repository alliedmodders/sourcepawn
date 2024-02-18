// vim: set ts=8 sts=4 sw=4 tw=99 et:
//  Pawn compiler - Recursive descend expresion parser
//
//  Copyright (c) AlliedModders LLC 2023
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

#pragma once

#include "compile-context.h"
#include "parse-node.h"
#include "symbols.h"
#include "types.h"

namespace sp {
namespace cc {

inline bool value::canRematerialize() const {
    switch (ident) {
        case iVARIABLE:
        case iCONSTEXPR:
            return true;
        default:
            return false;
    }
}

} // namespace cc
} // namespace sp
