// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
// Copyright (c) ITB CompuPhase, 1997-2005
//
#pragma once

#include "parse-node.h"

namespace sp {
namespace cc {

class Semantics;
struct typeinfo_t;

// Determine the static size of an iARRAY based on dimension expressions and
// array initializers. The array may be converted to an iREFARRAY if it is
// determined to be dynamic.
bool ResolveArrayType(Semantics* sema, VarDeclBase* decl);
bool ResolveArrayType(Semantics* sema, const token_pos_t& pos, typeinfo_t* type, int vclass);

// Perform type and size checks of an array and its initializer if present.
bool CheckArrayInitialization(Semantics* sema, const typeinfo_t& type, Expr* init);

} // namespace cc
} // namespace sp
