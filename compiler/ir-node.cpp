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

Rvalue::Rvalue(Value* operand)
  : Value(IrKind::Rvalue, operand->pn()),
    expr_(operand)
{
    auto& v = val();
    v = operand->val();
    if (v.ident == iACCESSOR) {
        if (v.accessor()->getter())
            markusage(v.accessor()->getter(), uREAD);
        v.ident = iEXPRESSION;
    }
    if (v.type()->isReference())
        v.set_type(v.type()->inner());
}

} // namespace ir
} // namespace cc
} // namespace sp
