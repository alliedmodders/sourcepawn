// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
//
#ifndef _include_sourcepawn_vm_api_h_
#define _include_sourcepawn_vm_api_h_

#include <amtl/am-cxx.h>
#include <sp_vm_api.h>

namespace sp {

using namespace SourcePawn;

extern size_t UTIL_Format(char* buffer, size_t maxlength, const char* fmt, ...);
extern size_t UTIL_FormatVA(char* buffer, size_t maxlength, const char* fmt, va_list ap);

} // namespace sp

#endif // _include_sourcepawn_vm_api_h_
