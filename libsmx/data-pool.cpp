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
