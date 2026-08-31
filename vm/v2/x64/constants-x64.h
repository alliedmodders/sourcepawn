// vim: set ts=8 sw=4 tw=99 sts=4 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

#include "macro-assembler.h"

namespace sp::v2 {

static const Register context_reg = r12;
/* r13 is used by env_reg in macro-assembler-x64 */
static const Register stk = r14;
static const Register dat_reg = r15;
static const Register frm = rbx;

} // namespace sp::v2
