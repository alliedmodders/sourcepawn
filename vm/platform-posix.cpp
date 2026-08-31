// vim: set sts=4 ts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#include "platform.h"

#include <pthread.h>
#include <unistd.h>

namespace sp {

#if !defined(KE_MACOSX)
intptr_t GetThreadStackLimit() {
    pthread_attr_t attr;
    if (pthread_getattr_np(pthread_self(), &attr) != 0)
        return 0;
    void* stackaddr = nullptr;
    size_t stacksize = 0;
    pthread_attr_getstack(&attr, &stackaddr, &stacksize);
    pthread_attr_destroy(&attr);
    if (!stackaddr)
        return 0;
    return static_cast<intptr_t>(reinterpret_cast<uintptr_t>(stackaddr));
}
#endif

intptr_t GetPageSize() {
    return static_cast<intptr_t>(sysconf(_SC_PAGESIZE));
}

} // namespace sp
