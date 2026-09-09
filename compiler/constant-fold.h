// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

#include <optional>

#include "types.h"
#include "value.h"

namespace sp {
namespace cc {

namespace ir {
class Value;
} // namespace ir

class BinaryExpr;
class Type;

std::optional<ExprVal> TryFoldBinary(BinaryExpr* expr, ir::Value* left, ir::Value* right,
                                     Type* type);

std::optional<bool> FoldToConstantBool(ir::Value* cond);

std::optional<ExprVal> TryFoldCast(const ExprVal& from, Type* to);
bool EvalConst(ir::Value* node, cell* value, Type** type);

} // namespace cc
} // namespace sp
