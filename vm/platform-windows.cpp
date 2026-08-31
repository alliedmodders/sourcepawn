// vim: set sts=4 ts=4 sw=4 tw=99 et:
//
// Copyright (C) 2026 AlliedModders LLC
//
// This file is part of SourcePawn.
//
// SourcePawn is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the
// Free Software Foundation, either version 3 of the License, or (at your
// option) any later version.
//
// SourcePawn is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License along
// with SourcePawn.  If not, see <http://www.gnu.org/licenses/>.
#include "platform.h"

#include <windows.h>

namespace sp {

intptr_t GetThreadStackLimit() {
    ULONG_PTR low = 0;
    ULONG_PTR high = 0;
    GetCurrentThreadStackLimits(&low, &high);
    if (high <= low)
        return 0;
    return static_cast<intptr_t>(static_cast<uintptr_t>(low) - GetPageSize());
}

intptr_t GetPageSize() {
    SYSTEM_INFO si;
    GetSystemInfo(&si);
    return static_cast<intptr_t>(si.dwPageSize);
}

} // namespace sp
