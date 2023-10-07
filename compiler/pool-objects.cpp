// vim: set ts=4 sw=4 tw=99 et:
//
// Copyright (C) 2012-2013 David Anderson
//
// This file is part of SourcePawn.
//
// SourcePawn is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option)
// any later version.
//
// SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
#include "pool-objects.h"

namespace sp {
namespace cc {

void
PoolAllocationPolicy::reportOutOfMemory()
{
    fprintf(stderr, "OUT OF POOL MEMORY\n");
    abort();
}

void
PoolAllocationPolicy::reportAllocationOverflow()
{
    fprintf(stderr, "OUT OF POOL MEMORY\n");
    abort();
}

void*
PoolAllocationPolicy::Malloc(size_t bytes)
{
    auto& cc = CompileContext::get();
    void* p = cc.allocator().rawAllocate(bytes);
    if (!p) {
        fprintf(stderr, "OUT OF POOL MEMORY\n");
        abort();
    }
    return p;
}

void PoolAllocationPolicy::Free(size_t bytes) {
    auto& cc = CompileContext::get();
    cc.allocator().trackFree(bytes);
}

void*
PoolAllocationPolicy::am_malloc(size_t bytes)
{
    return PoolAllocationPolicy::Malloc(bytes);
}

void
PoolAllocationPolicy::am_free(void* ptr)
{
}

} // namespace cc
} // namespace sp
