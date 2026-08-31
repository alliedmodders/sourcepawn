// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2012-2026 AlliedModders LLC
//
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
