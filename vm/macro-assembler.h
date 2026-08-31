// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
//
#ifndef _include_sourcepawn_macro_assembler_h__
#define _include_sourcepawn_macro_assembler_h__

#include <amtl/am-platform.h>

#if defined(KE_ARCH_X86)
#    include "x86/macro-assembler-x86.h"
#elif defined(KE_ARCH_X64)
#    include "x64/macro-assembler-x64.h"
#else
#    error "Unsupported architecture"
#endif

#endif // _include_sourcepawn_macro_assembler_h__
