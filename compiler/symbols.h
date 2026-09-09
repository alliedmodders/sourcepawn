// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2021-2026 AlliedModders LLC
// Copyright (c) ITB CompuPhase, 1997-2005

#pragma once

#include <functional>
#include <unordered_map>

#include "label.h"
#include "lexer.h"
#include "sc.h"
#include "source-location.h"
#include "stl/stl-unordered-map.h"
#include "value.h"

namespace sp {
namespace cc {

class CompileContext;
class Decl;

namespace ir {
class Lvalue;
} // namespace ir
class FunctionDecl;
class PropertyDecl;
class SemaContext;
struct token_pos_t;

enum ScopeKind {
    sGLOBAL = 0,      /* global variable/constant class (no states) */
    sLOCAL = 1,       /* local variable/constant */
    sSTATIC = 2,      /* global lifetime, local or global scope */
    sARGUMENT = 3,    /* function argument (this is never stored anywhere) */
    sENUMFIELD = 4,   /* for analysis purposes only (not stored anywhere) */
    sFILE_STATIC = 5, /* only appears on SymbolScope, to clarify sSTATIC */
    sCLASSFIELD = 6,  /* field of a class (heap-allocated object with GC) */
};

static inline bool IsLocal(int kind) {
    return kind == sLOCAL || kind == sARGUMENT;
}

static constexpr int kResolveType = (1 << 0);
static constexpr int kResolveIdent = (1 << 1);

struct ResolvedSymbol {
    Decl* decl = nullptr;
    SymbolScope* scope = nullptr;
    FunctionDecl* enclosure = nullptr;
    Type* type = nullptr;
};
bool ResolveSymbol(SemaContext* sc, SymbolScope* scope, Atom* name, ResolvedSymbol* resolved,
                   int flags);
Type* ResolveType(SemaContext& sc, Atom* name);

void DefineSymbol(SemaContext& sc, Decl* decl, int vclass);
bool CheckNameRedefinition(SemaContext& sc, Atom* name, const token_pos_t& pos, int vclass);
bool CheckTypeNameRedefinition(SemaContext& sc, Atom* name, const token_pos_t& pos);

void markusage(Decl* decl, int usage);
void markusage(ir::Lvalue* lval, int usage);
Decl* FindEnumStructField(Type* type, Atom* name);
Decl* FindClassField(Type* type, Atom* name);

} // namespace cc
} // namespace sp
