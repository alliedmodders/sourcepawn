// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2012-2026 AlliedModders LLC
//
#pragma once

#include <limits.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include <memory>
#include <new>
#include <span>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <amtl/am-bits.h>

namespace sp {

// Allocates memory in chunks that are not freed until the entire allocator
// is freed. This is intended for use with large, temporary data structures.
class PoolAllocator final
{
    friend class PoolAllocationScope;

    struct Pool {
        std::unique_ptr<char[]> base;
        char* ptr = nullptr;
        char* end = nullptr;

        size_t size() const {
            return size_t(end - base.get());
        }
    };

    static const size_t kDefaultPoolSize = 64 * 1024;
    static const size_t kMaxReserveSize = 64 * 1024;
    static const size_t kAlignment = sizeof(void*);

  private:
    std::vector<std::unique_ptr<Pool>> pools_;
    size_t wasted_ = 0;

  private:
    Pool* ensurePool(size_t actualBytes);

  public:
    PoolAllocator(size_t chunk_size = 0);
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
        size_t actualBytes = ke::Align(bytes, kAlignment);
        Pool* last = pools_.empty() ? nullptr : pools_.back().get();
        if (!last || (size_t(last->end - last->ptr) < actualBytes))
            last = ensurePool(actualBytes);
        char* ptr = last->ptr;
        last->ptr += actualBytes;
        return ptr;
    }

    void trackFree(size_t bytes) {
        wasted_ += bytes;
    }

    size_t wasted() const { return wasted_; }

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

    template <typename T, typename... Args>
    T* make(Args&&... args) {
        void* ptr = rawAllocate(sizeof(T));
        if (!ptr)
            return nullptr;
        return new (ptr) T(std::forward<Args>(args)...);
    }

    template <typename T>
    std::span<T> make_n(size_t count) {
        if (!ke::IsUintPtrMultiplySafe(count, sizeof(T))) {
            fprintf(stderr, "allocation overflow\n");
            return {};
        }
        void* ptr = rawAllocate(count * sizeof(T));
        if (!ptr)
            return {};

        T* elements = reinterpret_cast<T*>(ptr);
        for (size_t i = 0; i < count; i++)
            new (&elements[i]) T();

        return std::span<T>(elements, count);
    }

  private:
    size_t chunk_size_;
};

} // namespace sp
