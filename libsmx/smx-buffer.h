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
#ifndef _include_sp_smx_memory_buffer_h_
#define _include_sp_smx_memory_buffer_h_

#include <string.h>
#include <shared/byte-buffer.h>

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
