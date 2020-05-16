// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2012-2018 David Anderson
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

#ifndef _include_sourcepawn_metadata_byte_buffer_h
#define _include_sourcepawn_metadata_byte_buffer_h

#include <limits.h>
#include <stdlib.h>
#include <string.h>

#include <memory>
#include <utility>

#include <amtl/am-bits.h>

namespace sp {

// A growable byte buffer for data pools, code generation, etc.
class ByteBuffer
{
 public:
  ByteBuffer() {
    buffer_ = std::make_unique<uint8_t[]>(kInitialSize);
    buffer_pos_ = buffer_.get();
    buffer_end_ = buffer_pos_ + kInitialSize;
    oom_ = false;
  }

  template <typename T>
  bool write(const T& value) {
    if (!ensureSpace(sizeof(value)))
      return false;
    *reinterpret_cast<T*>(buffer_pos_) = value;
    buffer_pos_ += sizeof(value);
    return true;
  }

  bool writeBytes(const void* data, size_t length) {
    if (!ensureSpace(length))
      return false;
    memcpy(buffer_pos_, data, length);
    buffer_pos_ += length;
    return true;
  }

  bool preallocate(size_t bytes) {
    if (!ensureSpace(bytes))
      return false;
#if !defined(NDEBUG)
    memset(buffer_pos_, 0xcd, bytes);
#endif
    buffer_pos_ += bytes;
    return true;
  }

  template <typename T>
  T* ptr(uint32_t pos) {
    assert(pos < size_t(buffer_pos_ - buffer_.get()));
    return reinterpret_cast<T*>(buffer_.get() + pos);
  }

  uint32_t position() const {
    assert(buffer_pos_ - buffer_.get() <= INT_MAX);
    return uint32_t(buffer_pos_ - buffer_.get());
  }
  bool oom() const {
    return oom_;
  }
  uint8_t* bytes() const {
    return buffer_.get();
  }
  size_t size() const {
    return position();
  }

 private:
  bool ensureSpace(size_t size) {
    if (size_t(buffer_end_ - buffer_pos_) < size && !growBy(size)) {
      oom_ = true;
      return false;
    }
    assert(size_t(buffer_end_ - buffer_pos_) >= size);
    return true;
  }

  bool growBy(size_t extra) {
    size_t prev_size = size_t(buffer_end_ - buffer_.get());
    size_t used = size_t(buffer_pos_ - buffer_.get());

    if (ke::IsUintPtrAddSafe(prev_size, prev_size / 2)) {
      size_t new_size = prev_size + prev_size / 2;
      if (new_size - used >= extra && tryRealloc(new_size))
        return true;
    }
    if (!ke::IsUintPtrAddSafe(prev_size, extra))
      return false;
    return tryRealloc(prev_size + extra);
  }

  bool tryRealloc(size_t new_size) {
    // This would cause all sorts of problems for smx files.
    if (new_size > INT_MAX)
      return false;

    ptrdiff_t pos = buffer_pos_ - buffer_.get();
    std::unique_ptr<uint8_t[]> new_buffer = std::make_unique<uint8_t[]>(new_size);
    if (!new_buffer)
      return false;

    memcpy(new_buffer.get(), buffer_.get(), pos);
    buffer_ = std::move(new_buffer);
    buffer_pos_ = buffer_.get() + pos;
    buffer_end_ = buffer_.get() + new_size;
    return true;
  }

 private:
  static const size_t kInitialSize = 256;

 protected:
  std::unique_ptr<uint8_t[]> buffer_;
  uint8_t* buffer_pos_;
  uint8_t* buffer_end_;
  bool oom_;
};

} // namespace sp

#endif // _include_sourcepawn_metadata_byte_buffer_h
