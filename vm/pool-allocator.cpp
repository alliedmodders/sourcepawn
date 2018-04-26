/* vim: set ts=2 sw=2 tw=99 et:
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
#include <amtl/am-threadlocal.h>
#include "pool-allocator.h"

using namespace ke;
using namespace sp;

ThreadLocal<PoolAllocator*> sAllocatorTLS;

PoolAllocator::PoolAllocator()
 : reserved_(nullptr),
   last_(nullptr),
   scope_depth_(0)
{
}

PoolAllocator::~PoolAllocator()
{
  assert(!scope_depth_);

  unwind(nullptr);
  if (reserved_)
    free(reserved_);
}

void
PoolAllocator::InitDefault()
{
  assert(!sAllocatorTLS);
  sAllocatorTLS.set(new PoolAllocator());
}

PoolAllocator&
PoolAllocator::DefaultForThread()
{
  assert(sAllocatorTLS.get());
  return *sAllocatorTLS.get();
}

void
PoolAllocator::FreeDefault()
{
  if (!sAllocatorTLS)
    return;
  delete sAllocatorTLS.get();
  sAllocatorTLS = nullptr;
}

char*
PoolAllocator::enter()
{
  scope_depth_++;
  if (!last_)
    return nullptr;
  return last_->ptr;
}

void
PoolAllocator::leave(char* pos)
{
  assert(scope_depth_);
  unwind(pos);
  scope_depth_--;
}

void
PoolAllocator::unwind(char* pos)
{
  while (last_) {
    if (pos && pos >= last_->base && pos < last_->end)
      break;
    Pool* prev = last_->prev;
    {
      if (last_->size() <= kMaxReserveSize &&
        (!reserved_ || reserved_->size() < last_->size()))
      {
        if (reserved_)
          free(reserved_);
        reserved_ = last_;
      } else {
        free(last_);
      }
    }
    last_ = prev;
  }

  if (!last_) {
    assert(!pos);
    return;
  }

  last_->ptr = pos;
}

void*
PoolAllocator::slowAllocate(size_t actualBytes)
{
  size_t bytesNeeded = actualBytes + sizeof(Pool);
  if (bytesNeeded < kDefaultPoolSize)
    bytesNeeded = kDefaultPoolSize;

  Pool* pool;
  if (reserved_ && reserved_->size() >= bytesNeeded) {
    pool = reserved_;
    reserved_ = nullptr;
  } else {
    pool = (Pool*)malloc(bytesNeeded);
    if (!pool) {
      fprintf(stderr, "OUT OF POOL MEMORY\n");
      abort();
      return nullptr;
    }
    pool->base = (char*)(pool + 1);
    pool->end = (char*)pool + bytesNeeded;
  }
  pool->ptr = pool->base + actualBytes;
  pool->prev = last_;
  last_ = pool;
  return pool->base;
}

PoolScope::PoolScope()
 : pool_(PoolAllocator::DefaultForThread()),
   position_(pool_.enter())
{
}

PoolScope::PoolScope(PoolAllocator& allocator)
 : pool_(allocator),
   position_(pool_.enter())
{
}

PoolScope::~PoolScope()
{
  pool_.leave(position_);
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
PoolAllocationPolicy::am_malloc(size_t bytes)
{
  void* p = sAllocatorTLS.get()->rawAllocate(bytes);
  if (!p)
    reportOutOfMemory();
  return p;
}

void
PoolAllocationPolicy::am_free(void* ptr)
{
}

