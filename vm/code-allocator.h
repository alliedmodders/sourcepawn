// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
//
#ifndef _include_sourcepawn_code_allocator_h_
#define _include_sourcepawn_code_allocator_h_

#include <amtl/am-refcounting.h>
#include <amtl/am-vector.h>
#include <stddef.h>
#include <stdint.h>

namespace sp {

using namespace ke;

// Manages CodeChunks, optimized for the underlying system allocator.
class CodePool : public ke::Refcounted<CodePool>
{
    friend class CodeAllocator;

  public:
    ~CodePool();

  private:
    CodePool(uint8_t* start, size_t size);

    static RefPtr<CodePool> AllocateFor(size_t bytes);

    uint8_t* allocate(size_t bytes);
    size_t bytesFree() const {
        return end_ - ptr_;
    }

  private:
    CodePool(const CodePool&) = delete;
    void operator=(const CodePool&) = delete;

  private:
    uint8_t* start_;
    uint8_t* ptr_;
    uint8_t* end_;
    size_t size_;
};

// Raw reference to allocated code.
struct CodeChunk {
    CodeChunk()
     : address_(nullptr)
     , bytes_(0) {
    }
    CodeChunk(RefPtr<CodePool> pool, uint8_t* address, size_t bytes)
     : pool_(pool)
     , address_(address)
     , bytes_(bytes) {
    }

    uint8_t* address() const { return address_; }
    size_t bytes() const { return bytes_; }

    explicit operator bool() const { return !!address_; }

  private:
    RefPtr<CodePool> pool_;
    uint8_t* address_;
    size_t bytes_;
};

// Manages CodePools.
class CodeAllocator
{
  public:
    CodeAllocator();
    ~CodeAllocator();

    CodeChunk Allocate(size_t bytes);

  private:
    RefPtr<CodePool> newPool(size_t bytes);
    RefPtr<CodePool> findPool(size_t bytes);
    CodeChunk allocateInPool(RefPtr<CodePool> pool, size_t bytes);

  private:
    CodeAllocator(const CodeAllocator&) = delete;
    void operator=(const CodeAllocator&) = delete;

  private:
    std::vector<RefPtr<CodePool>> cached_pools_;
};

} // namespace sp

#endif // _sourcepawn_code_allocator_h_
