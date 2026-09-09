// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2021-2026 AlliedModders LLC
// Copyright (c) ITB CompuPhase, 1997-2005
//
#pragma once

#include "sc.h"
#include "types.h"

namespace sp {
namespace cc {

class Decl;
class FunctionDecl;
class LayoutFieldDecl;
class PropertyDecl;
class UpvarDecl;
class VarDeclBase;

struct ExprVal {
    ExprVal() : type_(nullptr) {}

    QualType type_;

    Type* type() const { return *type_; }
    QualType qualified() const { return type_; }
    void set_type(Type* type) { type_ = QualType(type); }
    void set_type(QualType type) { type_ = type; }

    void set_expr(QualType type) { set_type(type); }
};

static inline ExprVal ExpressionVal(QualType type) {
    ExprVal v;
    v.set_expr(type);
    return v;
}

} // namespace cc
} // namespace sp
