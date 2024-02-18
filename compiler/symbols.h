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

#pragma once

#include <functional>
#include <unordered_map>

#include "label.h"
#include "lexer.h"
#include "sc.h"
#include "source-location.h"
#include "stl/stl-unordered-map.h"

namespace sp {
namespace cc {

class CompileContext;
class Decl;
class FunctionDecl;
class MethodmapPropertyDecl;
class SemaContext;
struct token_pos_t;

enum ScopeKind {
    sGLOBAL = 0,      /* global variable/constant class (no states) */
    sLOCAL = 1,       /* local variable/constant */
    sSTATIC = 2,      /* global lifetime, local or global scope */
    sARGUMENT = 3,    /* function argument (this is never stored anywhere) */
    sENUMFIELD = 4,   /* for analysis purposes only (not stored anywhere) */
    sFILE_STATIC = 5, /* only appears on SymbolScope, to clarify sSTATIC */
};

struct value {
    value() : ident(iINVALID), sym(nullptr), type_(nullptr) {}

    IdentifierKind ident : 6;
    Decl* sym;
    Type* type_;

    Type* type() const { return type_; }
    void set_type(Type* type) { type_ = type; }

    // Returns whether the value can be rematerialized based on static
    // information, or whether it is the result of an expression.
    bool canRematerialize() const;

    MethodmapPropertyDecl* accessor() const {
        if (ident != iACCESSOR)
            return nullptr;
        return accessor_;
    }
    void set_accessor(MethodmapPropertyDecl* accessor) {
        ident = iACCESSOR;
        accessor_ = accessor;
    }
    cell constval() const {
        assert(ident == iCONSTEXPR);
        return constval_;
    }
    void set_constval(cell val) {
        ident = iCONSTEXPR;
        constval_ = val;
    }

    void set_slice(IdentifierKind ident, Decl* sym) {
        this->ident = ident;
        this->sym = sym;
    }

    union {
        // when ident == iACCESSOR
        MethodmapPropertyDecl* accessor_;
        // when ident == iCONSTEXPR
        cell constval_;
    };

    static value ErrorValue() {
        value v = {};
        v.ident = iCONSTEXPR;
        return v;
    }
};

static inline bool IsLocal(int kind) {
    return kind == sLOCAL || kind == sARGUMENT;
}

void DefineSymbol(SemaContext& sc, Decl* decl, int vclass);
bool CheckNameRedefinition(SemaContext& sc, Atom* name, const token_pos_t& pos, int vclass);

void markusage(Decl* decl, int usage);
Decl* FindEnumStructField(Type* type, Atom* name);

} // namespace cc
} // namespace sp
