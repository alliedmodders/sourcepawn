// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2006-2015 AlliedModders LLC
//
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
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
