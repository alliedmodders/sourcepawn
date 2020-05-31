/* vim: set ts=4 sw=4 tw=99 et:
 *
 * Copyright (C) 2012-2013 David Anderson
 *
 * This file is part of SourcePawn.
 *
 * SourcePawn is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 *
 * SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * SourcePawn. If not, see http://www.gnu.org/licenses/.
 */
#include <assert.h>
#include <stdlib.h>

#include <new>
#include <utility>

#include "pool-allocator.h"

using namespace ke;

PoolAllocator gPoolAllocator;

PoolAllocator::PoolAllocator()
{
}

PoolAllocator::~PoolAllocator()
{
    unwind(nullptr);
}

char*
PoolAllocator::enter()
{
    if (pools_.empty())
        return nullptr;
    return pools_.back()->ptr;
}

void
PoolAllocator::leave(char* pos)
{
    unwind(pos);
}

void
PoolAllocator::unwind(char* pos)
{
    while (!pools_.empty()) {
        Pool* last = pools_.back().get();
        if (pos && pos >= last->base.get() && pos < last->end) {
            last->ptr = pos;
            return;
        }
        pools_.pop_back();
    }
}

PoolAllocator::Pool*
PoolAllocator::ensurePool(size_t actualBytes)
{
    size_t bytesNeeded = actualBytes;
    if (bytesNeeded < kDefaultPoolSize)
        bytesNeeded = kDefaultPoolSize;

    auto pool = std::make_unique<Pool>();
    pool->base = std::make_unique<char[]>(bytesNeeded);
    pool->ptr = pool->base.get();
    pool->end = pool->ptr + bytesNeeded;
    pools_.push_back(std::move(pool));
    return pools_.back().get();
}

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
    void* p = gPoolAllocator.rawAllocate(bytes);
    if (!p) {
        fprintf(stderr, "OUT OF POOL MEMORY\n");
        abort();
    }
    return p;
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
