// vim: set ts=8 sw=4 tw=99 sts=4 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

#include <memory>

#include <amtl/am-refcounting.h>

namespace sp::v2 {

class ControlFlowGraph;
class LLCode;
class MethodInfo;

std::unique_ptr<LLCode> LowerMethod(ControlFlowGraph* graph, MethodInfo* method);

} // namespace sp::v2
