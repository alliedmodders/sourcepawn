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
