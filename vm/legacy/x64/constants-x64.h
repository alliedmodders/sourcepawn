// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#ifndef _include_sourcepawn_vm_legacy_constants_x64_h__
#define _include_sourcepawn_vm_legacy_constants_x64_h__

#include "x64/assembler-x64.h"

namespace sp::v1 {

// We prioritize rbx for being non-volatile and not needing an REX encoding,
// and r14/r15 for being non-volatile and not conflicting with mod r/m
// encoding.
static const Register pri = rax;
static const Register alt = rdx;
static const Register context_reg = r12;
static const Register stk = r14;
static const Register dat = r15;
static const Register frm = rbx;
static const Register tmp = rcx;

} // namespace sp::v1

#endif //_include_sourcepawn_vm_legacy_constants_x64_h__
