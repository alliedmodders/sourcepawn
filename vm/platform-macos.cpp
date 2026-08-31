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

intptr_t GetThreadStackLimit() {
    uintptr_t stackaddr =
        reinterpret_cast<uintptr_t>(pthread_get_stackaddr_np(pthread_self()));
    size_t stacksize = pthread_get_stacksize_np(pthread_self());
    return static_cast<intptr_t>(stackaddr - stacksize);
}

} // namespace sp
