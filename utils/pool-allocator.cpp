// vim: set ts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2012-2026 AlliedModders LLC
//
#include <assert.h>
#include <stdlib.h>

#include <new>
#include <utility>

#include "pool-allocator.h"

namespace sp {

PoolAllocator::PoolAllocator(size_t chunk_size)
{
    chunk_size_ = chunk_size;
    if (!chunk_size_)
        chunk_size_ = kDefaultPoolSize;
}

PoolAllocator::~PoolAllocator()
{
}

PoolAllocator::Pool*
PoolAllocator::ensurePool(size_t actualBytes)
{
    size_t bytesNeeded = actualBytes;
    if (bytesNeeded < chunk_size_)
        bytesNeeded = chunk_size_;

    auto pool = std::make_unique<Pool>();
    pool->base = std::make_unique<char[]>(bytesNeeded);
    pool->ptr = pool->base.get();
    pool->end = pool->ptr + bytesNeeded;
    pools_.push_back(std::move(pool));
    return pools_.back().get();
}

} // namespace sp
