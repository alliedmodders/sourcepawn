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

#ifndef _include_sourcepawn_metadata_datapool_h
#define _include_sourcepawn_metadata_datapool_h

#include <stdint.h>
#include <string.h>

#include <memory>
#include <utility>

#include <amtl/am-hashmap.h>
#include <amtl/am-vector.h>
#include "shared/byte-buffer.h"

namespace sp {

using namespace ke;

class DataPool
{
 public:
  DataPool();

  uint32_t add(const std::vector<uint8_t>& run);

  const ByteBuffer& buffer() const {
    return buffer_;
  }

 private:
  struct ByteRun {
    ByteRun()
    {}
    ByteRun(ByteRun&& other)
     : bytes(std::move(other.bytes)),
       length(other.length)
    {}
    std::unique_ptr<uint8_t[]> bytes;
    size_t length;
  };
  struct BytesAndLength {
    const uint8_t* bytes;
    size_t length;
  };

  struct ByteRunPolicy {
    static uint32_t hash(const BytesAndLength& key) {
      return HashCharSequence(reinterpret_cast<const char*>(key.bytes), key.length);
    }

    static bool matches(const BytesAndLength& key, const ByteRun& payload) {
      if (key.length != payload.length)
        return false;
      return memcmp(key.bytes, payload.bytes.get(), key.length) == 0;
    }
  };

  ByteBuffer buffer_;

  typedef HashMap<ByteRun, uint32_t, ByteRunPolicy> DataPoolMap;
  DataPoolMap pool_map_;
};

} // namespace sp

#endif // _include_sourcepawn_metadata_datapool_h
