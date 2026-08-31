// vim: set ts=8 sw=4 tw=99 sts=4 et:
//
// This file is part of SourcePawn.
//
// SourcePawn is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// SourcePawn is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with SourcePawn.  If not, see <http://www.gnu.org/licenses/>.
#pragma once

#include <stddef.h>
#include <stdint.h>

#include <algorithm>
#include <memory>
#include <vector>

#include <utils/bitset.h>

namespace sp::v2 {

class InterpCode
{
  public:
    struct OffsetMapping {
        uint32_t low;
        uint32_t high;
    };

    InterpCode(std::unique_ptr<uint8_t[]> bytes, size_t size, uint32_t num_regs,
               std::vector<OffsetMapping>&& mappings, BitSet&& gcobj_regs = BitSet())
     : bytes_(std::move(bytes)),
       size_(size),
       num_regs_(num_regs),
       mappings_(std::move(mappings)),
       gcobj_regs_(std::move(gcobj_regs))
    {
        mappings_.shrink_to_fit();
        gcobj_regs_.shrink_to_fit();
    }

    const uint8_t* bytes() const { return bytes_.get(); }
    size_t size() const { return size_; }
    uint32_t num_regs() const { return num_regs_; }
    const BitSet& gcobj_regs() const { return gcobj_regs_; }

    uint32_t LookupHighOffset(uint32_t low_offset) const {
        if (mappings_.empty())
            return 0;
        auto it = std::lower_bound(mappings_.begin(), mappings_.end(), low_offset,
            [](const OffsetMapping& m, uint32_t val) {
                return m.low < val;
            });
        if (it != mappings_.end() && it->low == low_offset)
            return it->high;
        if (it != mappings_.begin())
            return (it - 1)->high;
        return mappings_[0].high;
    }

  private:
    std::unique_ptr<uint8_t[]> bytes_;
    size_t size_;
    uint32_t num_regs_;
    std::vector<OffsetMapping> mappings_;
    BitSet gcobj_regs_;
};

} // namespace sp::v2
