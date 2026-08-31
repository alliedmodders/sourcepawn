// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2012-2026 AlliedModders LLC
//
#ifndef _include_sp_smx_memory_buffer_h_
#define _include_sp_smx_memory_buffer_h_

#include <string.h>
#include <utils/byte-buffer.h>

namespace sp {

// Interface for SmxBuilder to blit bytes.
class ISmxBuffer
{
 public:
  virtual bool write(const void* bytes, size_t len) = 0;
  virtual size_t pos() const = 0;
};

class SmxByteBuffer
 : public ISmxBuffer,
   public ByteBuffer
{
 public:
  bool write(const void* bytes, size_t len) override {
    return ByteBuffer::writeBytes(bytes, len);
  }
  size_t pos() const override {
    return ByteBuffer::position();
  }
};

} // namespace sp

#endif // _include_sp_smx_memory_buffer_h_
