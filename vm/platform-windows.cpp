// vim: set sts=4 ts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
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
