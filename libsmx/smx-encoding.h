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
#ifndef _include_sp_smx_encoding_h_
#define _include_sp_smx_encoding_h_

#include <stdint.h>
#include <amtl/am-vector.h>

namespace sp {

static inline void
CompactEncodeUint32(std::vector<uint8_t>& out, uint32_t value)
{
  uint32_t copy = value;
  do {
    uint8_t byte = uint8_t(copy & 0x7f);
    if (copy > 0x7f)
      byte |= 0x80;
    out.push_back(byte);
    copy >>= 7;
  } while (copy);
}

} // namespace sp

#endif // _include_sp_smx_encoding_h_
