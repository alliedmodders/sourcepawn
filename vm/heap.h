// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// Copyright (C) 2026 AlliedModders LLC
//
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#pragma once

#include <assert.h>
#include <stddef.h>
#include <stdint.h>

#include <new>
#include <utility>
#include <string>
#include <type_traits>

#include <amtl/am-bits.h>
#include <amtl/am-refcounting.h>
#include "handle.h"
#include "objects.h"
#include "type-desc.h"
#if defined(KE_64BIT)
# include "virtmem-64bit.h"
#elif defined(KE_32BIT)
# include "virtmem-32bit.h"
#else
# error "KE_64BIT or KE_32BIT not defined"
#endif

struct mi_heap_s;
typedef struct mi_heap_s mi_heap_t;

namespace sp {

#ifdef KE_64BIT
using VirtMem = VirtMem64;
#else
using VirtMem = VirtMem32;
#endif

static constexpr uint32_t kDefaultStackSize = 1 * ke::kMB;
static constexpr uint32_t kDefaultHeapChunkSize = 16 * ke::kMB;

template <typename T>
class RawHeapPtr;

class RawHeap {
  public:
    RawHeap(VirtMem& virt_mem);
    ~RawHeap();

    bool Initialize();

    uint32_t ToLocalAddr(void* p) { return virt_mem_.ToLocalAddr(p); }

    template <typename T>
    T ToPhysAddr(uint32_t addr) {
        return virt_mem_.ToPhysAddr<T>(addr);
    }

    void* AllocRaw(size_t bytes);
    void FreeRaw(void* ptr);

    template <typename T, typename... Args>
    typename std::enable_if<!std::is_array<T>::value, RawHeapPtr<T>>::type MakeRawPtr(Args&&... args);

    template <typename T>
    typename std::enable_if<std::is_array<T>::value, RawHeapPtr<T>>::type MakeRawPtr(size_t n);

  protected:
    VirtMem& virt_mem_;
    mi_heap_t* mi_heap_ = nullptr;
};

class Heap : public RawHeap {
  public:
    using RawHeap::RawHeap;

    template <typename T> Handle<T> New(const TypeDesc* td, uint32_t payload_bytes = 0) {
        static_assert(std::is_base_of_v<HeapItem, T>, "Must be derived from HeapItem");
        assert(ke::IsUintAddSafe(static_cast<uint32_t>(sizeof(T)), payload_bytes));

        auto obj = reinterpret_cast<HeapItem*>(AllocRaw(sizeof(T) + payload_bytes));
        if (!obj)
            return {};

        obj->td = td;
        obj->rc = 0;
        return Handle<T>(reinterpret_cast<T*>(obj));
    }

    std::string LiveObjectReport() const;
};

template <typename T>
class RawHeapPtr {
  public:
    RawHeapPtr() : heap_(nullptr), ptr_(nullptr) {}
    RawHeapPtr(RawHeap& heap, T* ptr) : heap_(&heap), ptr_(ptr) {}
    ~RawHeapPtr() {
        reset();
    }

    RawHeapPtr(const RawHeapPtr&) = delete;
    RawHeapPtr& operator=(const RawHeapPtr&) = delete;

    RawHeapPtr(RawHeapPtr&& other) noexcept
      : heap_(other.heap_),
        ptr_(other.ptr_)
    {
        other.heap_ = nullptr;
        other.ptr_ = nullptr;
    }

    RawHeapPtr& operator=(RawHeapPtr&& other) noexcept {
        if (this != &other) {
            reset();
            heap_ = other.heap_;
            ptr_ = other.ptr_;
            other.heap_ = nullptr;
            other.ptr_ = nullptr;
        }
        return *this;
    }

    explicit operator bool() const { return ptr_ != nullptr; }

    T* get() const { return ptr_; }
    T* operator->() const { return ptr_; }
    T& operator*() const { return *ptr_; }

    T* release() {
        T* p = ptr_;
        ptr_ = nullptr;
        heap_ = nullptr;
        return p;
    }

    void reset(T* ptr = nullptr) {
        if (ptr_ == ptr) {
            return;
        }
        if (ptr_) {
            ptr_->~T();
            heap_->FreeRaw(ptr_);
        }
        ptr_ = ptr;
        if (!ptr_) {
            heap_ = nullptr;
        }
    }

  private:
    RawHeap* heap_;
    T* ptr_;
};

template <typename T>
class RawHeapPtr<T[]> {
  public:
    using element_type = typename std::remove_extent<T>::type;

    RawHeapPtr() : heap_(nullptr), ptr_(nullptr), size_(0) {}
    RawHeapPtr(RawHeap& heap, element_type* ptr, size_t size) : heap_(&heap), ptr_(ptr), size_(size) {}
    ~RawHeapPtr() {
        reset();
    }

    RawHeapPtr(const RawHeapPtr&) = delete;
    RawHeapPtr& operator=(const RawHeapPtr&) = delete;

    RawHeapPtr(RawHeapPtr&& other) noexcept
      : heap_(other.heap_),
        ptr_(other.ptr_),
        size_(other.size_)
    {
        other.heap_ = nullptr;
        other.ptr_ = nullptr;
        other.size_ = 0;
    }

    RawHeapPtr& operator=(RawHeapPtr&& other) noexcept {
        if (this != &other) {
            reset();
            heap_ = other.heap_;
            ptr_ = other.ptr_;
            size_ = other.size_;
            other.heap_ = nullptr;
            other.ptr_ = nullptr;
            other.size_ = 0;
        }
        return *this;
    }

    explicit operator bool() const { return ptr_ != nullptr; }

    element_type* get() const { return ptr_; }
    element_type& operator[](size_t index) const { return ptr_[index]; }

    element_type* release() {
        element_type* p = ptr_;
        ptr_ = nullptr;
        heap_ = nullptr;
        size_ = 0;
        return p;
    }

    void reset(element_type* ptr = nullptr, size_t size = 0) {
        if (ptr_ == ptr) {
            return;
        }
        if (ptr_) {
            for (size_t i = 0; i < size_; ++i) {
                ptr_[i].~element_type();
            }
            heap_->FreeRaw(ptr_);
        }
        ptr_ = ptr;
        size_ = size;
        if (!ptr_) {
            heap_ = nullptr;
        }
    }

  private:
    RawHeap* heap_;
    element_type* ptr_;
    size_t size_;
};

template <typename T, typename... Args>
inline typename std::enable_if<!std::is_array<T>::value, RawHeapPtr<T>>::type
RawHeap::MakeRawPtr(Args&&... args) {
    void* mem = AllocRaw(sizeof(T));
    if (!mem) {
        return RawHeapPtr<T>();
    }
    T* ptr = ::new (mem) T(std::forward<Args>(args)...);
    return RawHeapPtr<T>(*this, ptr);
}

template <typename T>
inline typename std::enable_if<std::is_array<T>::value, RawHeapPtr<T>>::type
RawHeap::MakeRawPtr(size_t n) {
    using ElementType = typename std::remove_extent<T>::type;
    void* mem = AllocRaw(sizeof(ElementType) * n);
    if (!mem) {
        return RawHeapPtr<T>();
    }
    ElementType* ptr = static_cast<ElementType*>(mem);
    for (size_t i = 0; i < n; ++i) {
        ::new (&ptr[i]) ElementType();
    }
    return RawHeapPtr<T>(*this, ptr, n);
}

} // namespace sp
