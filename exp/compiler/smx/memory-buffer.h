// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2012-2014 AlliedModders LLC, David Anderson
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
#ifndef _include_sp_memory_buffer_h_
#define _include_sp_memory_buffer_h_

#include <stdio.h>

namespace sp {

// Interface for SmxBuilder to blit bytes.
class ISmxBuffer
{
 public:
  virtual bool write(const void *bytes, size_t len) = 0;
  virtual size_t pos() const = 0;
};

// An in-memory buffer for SmxBuilder.
class MemoryBuffer : public ISmxBuffer
{
  static const size_t kDefaultSize = 4096;

 public:
  MemoryBuffer() {
    buffer_ = (uint8_t *)calloc(kDefaultSize, 1);
    pos_ = buffer_;
    end_ = buffer_ + kDefaultSize;
    oom_ = !buffer_;
  }
  ~MemoryBuffer() {
    free(buffer_);
  }

  // :TODO: handle oom

  template <typename T>
  bool write(const T& value) {
    return write(&value, sizeof(T));
  }

  bool write(const void *bytes, size_t len) override {
    // :TODO: fix for overflow
    if (pos_ + len > end_) {
      if (!grow(len))
        return false;
    }
    memcpy(pos_, bytes, len);
    pos_ += len;
    return true;
  }

  size_t pos() const override {
    return pos_ - buffer_;
  }

  uint8_t *bytes() const {
    return buffer_;
  }
  size_t size() const {
    return pos();
  }
  bool outOfMemory() const {
    return oom_;
  }

 private:
  bool grow(size_t len) {
    if (!grow_impl(len)) {
      oom_ = true;
      return false;
    }
    return true;
  }
  bool grow_impl(size_t len) {
    if (!ke::IsUintPtrAddSafe(pos(), len))
      return false;

    size_t new_maxsize = end_ - buffer_;
    while (pos() + len > new_maxsize) {
      if (!ke::IsUintPtrMultiplySafe(new_maxsize, 2))
        return false;
      new_maxsize *= 2;
    }

    uint8_t *newbuffer = (uint8_t *)realloc(buffer_, new_maxsize);
    if (!newbuffer)
      return false;
    pos_ = newbuffer + (pos_ - buffer_);
    end_ = newbuffer + new_maxsize;
    buffer_ = newbuffer;
    return true;
  }

 private:
  uint8_t *buffer_;
  uint8_t *pos_;
  uint8_t *end_;
  bool oom_;
};

} // namespace sp

#endif // _include_sp_memory_buffer_h_
