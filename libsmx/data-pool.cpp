// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2012-2026 AlliedModders LLC

#include "data-pool.h"

#include <utility>

namespace sp { 

DataPool::DataPool()
{
  pool_map_.init(64);
  buffer_.write<uint8_t>(0);
}

uint32_t
DataPool::add(const std::vector<uint8_t>& run)
{
  BytesAndLength tmp_key;
  tmp_key.bytes = run.data();
  tmp_key.length = run.size();

  DataPoolMap::Insert p = pool_map_.findForAdd(tmp_key);
  if (p.found())
    return p->value;

  uint32_t index = buffer_.position();
  if (!buffer_.writeBytes(run.data(), run.size()))
    return 0;

  ByteRun key;
  key.bytes = std::make_unique<uint8_t[]>(run.size());
  key.length = run.size();
  memcpy(key.bytes.get(), run.data(), key.length);
  pool_map_.add(p, std::move(key), index);
  return index;
}

} // namespace sp
