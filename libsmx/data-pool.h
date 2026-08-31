// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2012-2026 AlliedModders LLC

#ifndef _include_sourcepawn_metadata_datapool_h
#define _include_sourcepawn_metadata_datapool_h

#include <stdint.h>
#include <string.h>

#include <memory>
#include <utility>

#include <amtl/am-hashmap.h>
#include <amtl/am-vector.h>
#include "utils/byte-buffer.h"

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
