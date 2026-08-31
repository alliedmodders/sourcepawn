// vim: set ts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2012-2026 AlliedModders LLC
//
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
