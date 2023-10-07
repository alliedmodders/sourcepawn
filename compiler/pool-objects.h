// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// Copyright (C) 2021 AlliedModders LLC
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

#include <forward_list>

#include "compile-context.h"

#include <amtl/am-fixedarray.h>

namespace sp {
namespace cc {

class PoolObject
{
  public:
    void* operator new(size_t size) {
	auto& cc = CompileContext::get();
        return cc.allocator().rawAllocate(size);
    }
    void* operator new [](size_t size) {
	auto& cc = CompileContext::get();
        return cc.allocator().rawAllocate(size);
    }

    // Using delete on pool-allocated objects is illegal.
    void operator delete(void* ptr) {
        assert(false);
    }
    void operator delete [](void* ptr) {
        assert(false);
    }
};

class PoolString : public PoolObject
{
  public:
    explicit PoolString(const std::string& other) {
	auto& cc = CompileContext::get();
        length_ = other.size();
        chars_ = (char*)cc.allocator().rawAllocate(length_ + 1);
        memcpy(chars_, other.c_str(), length_ + 1);
    }
    PoolString(const char* chars, size_t len) {
	auto& cc = CompileContext::get();
        length_ = len;
        chars_ = (char*)cc.allocator().rawAllocate(length_ + 1);
        memcpy(chars_, chars, length_ + 1);
    }

    const char* chars() const {
        return chars_;
    }
    size_t length() const {
        return length_;
    }

  private:
    char* chars_;
    size_t length_;
};

class PoolAllocationPolicy
{
  protected:
    void reportAllocationOverflow();
    void reportOutOfMemory();

  public:
    void* am_malloc(size_t bytes);
    void am_free(void* ptr);

    static void* Malloc(size_t bytes);
    static void Free(size_t bytes);
};

template <typename T>
class StlPoolAllocator
{
  public:
    typedef T value_type;
    typedef std::true_type propagate_on_container_move_assignment;
    typedef std::true_type propagate_on_container_copy_assignment;
    typedef std::true_type propagate_on_container_swap;
    typedef std::true_type is_always_equal;

    StlPoolAllocator() = default;
    StlPoolAllocator(const StlPoolAllocator&) = default;

    template <typename U>
    StlPoolAllocator(const StlPoolAllocator<U>& other) {}

    template <typename U>
    using rebind = StlPoolAllocator<U>;

    static T* allocate(size_t n, const void* = nullptr) {
        if (!ke::IsUintMultiplySafe(n, sizeof(T)))
            throw std::bad_alloc{};
        return reinterpret_cast<T*>(PoolAllocationPolicy::Malloc(n * sizeof(T)));
    }
    void deallocate(T* p, size_t n) {
        PoolAllocationPolicy::Free(sizeof(T) * n);
    }

    bool operator ==(const StlPoolAllocator& other) const { return true; }
    bool operator !=(const StlPoolAllocator& other) const { return false; }
};

template <typename T>
using PoolList = std::vector<T, StlPoolAllocator<T>>;

template <typename T>
using PoolArray = ke::FixedArray<T, PoolAllocationPolicy>;

template <typename Key,
          typename T,
          typename Hash = std::hash<Key>,
          typename KeyEqual = std::equal_to<Key>>
using PoolMap = std::unordered_map<Key, T, Hash, KeyEqual, StlPoolAllocator<std::pair<const Key, T>>>;

template <typename T>
using PoolForwardList = std::forward_list<T, StlPoolAllocator<T>>;

struct KeywordTablePolicy {
    static bool matches(const sp::CharsAndLength& a, const sp::CharsAndLength& b) {
        if (a.length() != b.length())
            return false;
        return strncmp(a.str(), b.str(), a.length()) == 0;
    }
    static uint32_t hash(const sp::CharsAndLength& key) {
        return ke::HashCharSequence(key.str(), key.length());
    }
};

} // namespace cc
} // namespace sp
