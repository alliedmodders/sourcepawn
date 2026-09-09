// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2024-2026 AlliedModders LLC
//
#include "ir-node.h"

#include "symbols.h"

namespace sp {
namespace cc {
namespace ir {

static ExprVal RvalueVal(Value* operand) {
    ExprVal v = operand->val();
    if (auto* accessor = operand->as<Accessor>()) {
        if (accessor->accessor()->getter())
            markusage(accessor->accessor()->getter(), uREAD);
    }
    if (v.type()->isReference())
        v.set_type(v.type()->inner());
    return v;
}

Rvalue::Rvalue(Lvalue* operand)
  : Value(IrKind::Rvalue, operand->pn(), RvalueVal(operand)),
    expr_(operand)
{}

} // namespace ir
} // namespace cc
} // namespace sp
