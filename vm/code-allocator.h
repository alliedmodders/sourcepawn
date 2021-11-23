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
#ifndef _include_sourcepawn_code_allocator_h_
#define _include_sourcepawn_code_allocator_h_

#include <stddef.h>
#include <stdint.h>
#include <amtl/am-refcounting.h>
#include <amtl/am-vector.h>

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
  void operator =(const CodePool&) = delete;

 private:
  uint8_t* start_;
  uint8_t* ptr_;
  uint8_t* end_;
  size_t size_;
};

// Raw reference to allocated code.
struct CodeChunk
{
  CodeChunk()
   : address_(nullptr),
     bytes_(0)
  {}
  CodeChunk(RefPtr<CodePool> pool, uint8_t* address, size_t bytes)
   : pool_(pool),
     address_(address),
     bytes_(bytes)
  {}

  uint8_t* address() const {
    return address_;
  }
  size_t bytes() const {
    return bytes_;
  }

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
  void operator =(const CodeAllocator&) = delete;

 private:
  std::vector<RefPtr<CodePool>> cached_pools_;
};

} // namespace sp

#endif // _sourcepawn_code_allocator_h_
