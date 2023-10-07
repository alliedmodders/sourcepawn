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

namespace sp {

PoolAllocator::PoolAllocator()
{
}

PoolAllocator::~PoolAllocator()
{
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

} // namespace sp
