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
#ifndef _include_spcomp_assembly_buffer_h_
#define _include_spcomp_assembly_buffer_h_

#include <stdlib.h>

namespace sp {

class AssemblyBuffer
{
 public:
  AssemblyBuffer()
   : oom_(false),
     buffer_(nullptr),
     buffer_end_(nullptr)
  {
    buffer_ = (uint8_t*)malloc(kInitialSize);
    buffer_pos_ = buffer_;
    buffer_end_ = buffer_ + kInitialSize;
    oom_ = !buffer_;
  }
  ~AssemblyBuffer() {
    free(buffer_);
  }

  bool outOfMemory() const {
    return oom_;
  }
  uint8_t* buffer() const {
    return buffer_;
  }
  size_t buffer_length() const {
    return buffer_pos_ - buffer_;
  }
  uint32_t pc() const {
    return static_cast<uint32_t>(buffer_pos_ - buffer_);
  }

 protected:
  template <typename T>
  void write(const T& value) {
    if (!maybeResize(sizeof(T)))
      return;

    *reinterpret_cast<T*>(buffer_pos_) = value;
    buffer_pos_ += sizeof(T);

    assert(buffer_pos_ <= buffer_end_ && buffer_pos_ >= buffer_);
  }

  bool maybeResize(size_t size) {
    if (size_t(buffer_end_ - buffer_pos_) < size && !tryResize(size)) {
      oom_ = true;
      return false;
    }

    assert(size_t(buffer_end_ - buffer_pos_) >= size);
    return true;
  }

 private:
  bool tryResize(size_t extra) {
    size_t prev_size = size_t(buffer_end_ - buffer_);
    size_t used = size_t(buffer_pos_ - buffer_);

    if (IsUintPtrAddSafe(prev_size, prev_size / 2)) {
      size_t new_size = prev_size + prev_size / 2;
      if (new_size - used >= extra && tryRealloc(new_size))
        return true;
    }
    if (!IsUintPtrAddSafe(prev_size, extra))
      return false;
    return tryRealloc(prev_size + extra);
  }

  bool tryRealloc(size_t new_size) {
    // This would cause all sorts of problems.
    if (new_size > INT_MAX)
      return false;

    ptrdiff_t pos = buffer_pos_ - buffer_;
    uint8_t* new_buf = (uint8_t*)realloc(buffer_, new_size);
    if (!new_buf)
      return false;

    buffer_ = new_buf;
    buffer_pos_ = new_buf + pos;
    buffer_end_ = new_buf + new_size;
    return true;
  }

 private:
  static const size_t kInitialSize = 512;

 private:
  bool oom_;
  uint8_t* buffer_;
  uint8_t* buffer_pos_;
  uint8_t* buffer_end_;
};

} // namespace sp

#endif // _include_spcomp_assembly_buffer_h_
