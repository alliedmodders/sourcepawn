// vim: set sts=2 ts=8 sw=2 tw=99 et:
// 
// Copyright (C) 2006-2015 AlliedModders LLC
// 
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#include <assert.h>
#include "code-allocator.h"
#if defined(_WIN32)
# include <Windows.h>
#else
# include <unistd.h>
# include <sys/mman.h>
#endif
#include <amtl/am-bits.h>

using namespace sp;

static const size_t kMaxCachedPools = 8;

CodeAllocator::CodeAllocator()
{
}

CodeAllocator::~CodeAllocator()
{
}

CodeChunk
CodeAllocator::Allocate(size_t rawBytes)
{
  size_t bytes = ke::Align(rawBytes, ke::kMallocAlignment);
  if (bytes < rawBytes)
    return CodeChunk();

  // First search the cache for any pools we can re-use.
  RefPtr<CodePool> pool = findPool(bytes);
  if (pool)
    return allocateInPool(pool, bytes);

  pool = CodePool::AllocateFor(bytes);
  if (!pool)
    return CodeChunk();

  CodeChunk chunk = allocateInPool(pool, bytes);

  // Enter this pool into the cache if we can.
  if (cached_pools_.size() < kMaxCachedPools) {
    cached_pools_.push_back(pool);
  } else {
    // If this pool has more free space than any of our cached pools, then
    // evict the pool with the least amount of free space left.
    size_t min_index = 0;
    for (size_t i = 1; i < cached_pools_.size(); i++) {
      if (cached_pools_[i]->bytesFree() < cached_pools_[min_index]->bytesFree())
        min_index = i;
    }
    if (cached_pools_[min_index]->bytesFree() < pool->bytesFree())
      cached_pools_[min_index] = pool;
  }

  return chunk;
}

RefPtr<CodePool>
CodeAllocator::findPool(size_t bytes)
{
  // Find the cached pool with the smallest free region that holds |bytes|, to
  // reduce fragmentation.
  RefPtr<CodePool> min;
  for (size_t i = 0; i < cached_pools_.size(); i++) {
    RefPtr<CodePool> pool = cached_pools_[i];
    if (bytes > pool->bytesFree())
      continue;
    if (!min || pool->bytesFree() < min->bytesFree())
      min = pool;
  }
  return min;
}

CodeChunk
CodeAllocator::allocateInPool(RefPtr<CodePool> pool, size_t bytes)
{
  uint8_t* address = pool->allocate(bytes);
  return CodeChunk(pool, address, bytes);
}

static size_t kPageGranularity = 0;
static size_t kMinPoolSize = 1 * kMB;

RefPtr<CodePool>
CodePool::AllocateFor(size_t askBytes)
{
  if (!kPageGranularity) {
    // On Windows, the page granularity is defined as 64KB. On POSIX systems it's
    // usually 4KB.
#if defined(_WIN32)
    SYSTEM_INFO info;
    GetSystemInfo(&info);
    kPageGranularity = info.dwAllocationGranularity;
#else
    kPageGranularity = sysconf(_SC_PAGESIZE);
#endif
    assert(ke::IsAligned(kPageGranularity, kMallocAlignment));
  }

  // If the allocation is larger than our minimum pool size, we only align up
  // to the page granularity.
  size_t bytes = (askBytes < kMinPoolSize)
                 ? kMinPoolSize
                 : ke::Align(askBytes, kPageGranularity);
  assert(ke::IsAligned(bytes, kPageGranularity));

#if defined(_WIN32)
  void* address = (uint8_t* )VirtualAlloc(nullptr, bytes, MEM_COMMIT|MEM_RESERVE, PAGE_EXECUTE_READWRITE);
  if (!address)
    return nullptr;
#else
  void* address = mmap(nullptr, bytes, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_PRIVATE|MAP_ANON, -1, 0);
  if (address == MAP_FAILED)
    return nullptr;
#endif

  return new CodePool((uint8_t*)address, bytes);
}

CodePool::CodePool(uint8_t* start, size_t size)
 : start_(start),
   ptr_(start),
   end_(start + size),
   size_(size)
{
}

CodePool::~CodePool()
{
#if defined(_WIN32)
  VirtualFree(start_, 0, MEM_RELEASE);
#else
  munmap(start_, size_);
#endif
}

uint8_t*
CodePool::allocate(size_t bytes)
{
  assert(ptr_ + bytes <= end_);
  uint8_t* result = ptr_;
  ptr_ += bytes;
  return result;
}
