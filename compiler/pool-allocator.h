// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2012-2014 David Anderson
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
#ifndef _include_jitcraft_pool_allocator_h_
#define _include_jitcraft_pool_allocator_h_

#include <new>
#include <stddef.h>
#include <stdio.h>
#include <limits.h>
#include <amtl/am-fixedarray.h>
#include <amtl/am-uniqueptr.h>
#include <amtl/am-vector.h>

// Allocates memory in chunks that are not freed until the entire allocator
// is freed. This is intended for use with large, temporary data structures.
class PoolAllocator final
{
    friend class PoolAllocationScope;

    struct Pool {
        ke::UniquePtr<char[]> base;
        char* ptr = nullptr;
        char* end = nullptr;

        size_t size() const {
            return size_t(end - base.get());
        }
    };

    static const size_t kDefaultPoolSize = 8 * 1024;
    static const size_t kMaxReserveSize = 64 * 1024;

  private:
    ke::Vector<ke::UniquePtr<Pool>> pools_;

  private:
    void unwind(char* pos);
    Pool* ensurePool(size_t actualBytes);

  public:
    PoolAllocator();
    ~PoolAllocator();

    void memoryUsage(size_t* allocated, size_t* reserved, size_t* bookkeeping) const {
        *allocated = 0;
        *reserved = 0;
        *bookkeeping = 0;
        for (const auto& pool : pools_) {
            *allocated += size_t(pool->ptr - pool->base.get());
            *reserved += size_t(pool->end - pool->base.get());
            *bookkeeping += sizeof(Pool);
        }
    }

    void* rawAllocate(size_t bytes) {
        // Guarantee malloc alignment.
        size_t actualBytes = ke::Align(bytes, ke::kMallocAlignment);
        Pool* last = pools_.empty() ? nullptr : pools_.back().get();
        if (!last || (size_t(last->end - last->ptr) < actualBytes))
            last = ensurePool(actualBytes);
        char* ptr = last->ptr;
        last->ptr += actualBytes;
        return ptr;
    }

    template <typename T>
    T* alloc(size_t count = 1) {
        if (!ke::IsUintPtrMultiplySafe(count, sizeof(T))) {
            fprintf(stderr, "allocation overflow\n");
            return nullptr;
        }
        void* ptr = rawAllocate(count * sizeof(T));
        if (!ptr)
            return nullptr;

        return reinterpret_cast<T*>(ptr);
    }

    char* enter();
    void leave(char* position);
};

extern PoolAllocator gPoolAllocator;

class PoolScope final
{
    PoolAllocator* pool_;
    char* position_;

  public:
    PoolScope()
      : pool_(nullptr),
        position_(nullptr)
    {
    }
    explicit PoolScope(PoolAllocator& allocator)
      : pool_(&allocator),
        position_(allocator.enter())
    {}
    ~PoolScope() {
        if (pool_)
          pool_->leave(position_);
    }

    void enter(PoolAllocator& pool) {
        assert(!pool_);
        pool_ = &pool;
        position_ = pool.enter();
    }

  private:
    PoolScope(const PoolScope& other) = delete;
    PoolScope& operator =(const PoolScope& other) = delete;
};

class PoolObject
{
  public:
    void* operator new(size_t size) {
        return gPoolAllocator.rawAllocate(size);
    }
    void* operator new [](size_t size) {
        return gPoolAllocator.rawAllocate(size);
    }

    // Using delete on pool-allocated objects is illegal.
    void operator delete(void* ptr) {
        assert(false);
    }
    void operator delete [](void* ptr) {
        assert(false);
    }
};

class PoolAllocationPolicy
{
  protected:
    void reportAllocationOverflow();
    void reportOutOfMemory();

  public:
    void* am_malloc(size_t bytes);
    void am_free(void* ptr);
};

template <typename T>
class PoolList final : public ke::Vector<T, PoolAllocationPolicy>
{
};

#endif // _include_jitcraft_pool_allocator_h_
