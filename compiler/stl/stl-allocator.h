// vim: set sts=4 ts=8 sw=4 tw=99 et:
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
