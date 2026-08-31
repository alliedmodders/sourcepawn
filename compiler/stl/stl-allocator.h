// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2012-2026 AlliedModders LLC

#pragma once

#include <stddef.h>

#include <type_traits>

#include <amtl/am-bits.h>

namespace sp {
namespace cc {

class NativeAllocator
{
  public:
    static void* Malloc(size_t n);
    static void Free(void* p, size_t n);
};

template <typename T>
class StlAllocator
{
  public:
    typedef T value_type;
    typedef std::true_type propagate_on_container_move_assignment;
    typedef std::true_type propagate_on_container_copy_assignment;
    typedef std::true_type propagate_on_container_swap;
    typedef std::true_type is_always_equal;
    typedef std::size_t size_type;
    typedef std::ptrdiff_t difference_type;

    // Legacy definitions.
    typedef T& reference;
    typedef T* pointer;
    typedef const T* const_pointer;
    typedef const T& const_reference;

    StlAllocator() = default;
    StlAllocator(const StlAllocator&) = default;

    template <typename U>
    StlAllocator(const StlAllocator<U>& other) {}

    static T* allocate(size_t n, const void* = nullptr) {
        if (!ke::IsUintMultiplySafe(n, sizeof(T)))
            throw std::bad_alloc{};
        return reinterpret_cast<T*>(NativeAllocator::Malloc(n * sizeof(T)));
    }
    void deallocate(T* p, size_t n) {
        NativeAllocator::Free(p, sizeof(T) * n);
    }

    template<typename U>
    struct rebind {
      typedef StlAllocator<U> other;
    };

    bool operator ==(const StlAllocator& other) const { return true; }
    bool operator !=(const StlAllocator& other) const { return false; }
};

} // namespace cc
} // namespace sp
