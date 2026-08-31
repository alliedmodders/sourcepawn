// vim: set ts=8 sw=4 tw=99 sts=4 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

#include <stddef.h>
#include <stdint.h>

#include <algorithm>
#include <memory>
#include <span>
#include <vector>

#include <amtl/am-fixedarray.h>
#include <utils/bitset.h>

namespace sp::v2 {

struct LLBlock {
    std::span<const uint8_t> bytes;
    ke::FixedArray<uint32_t> successors;
};

static constexpr uint16_t LL_INVALID_REG = 0xFFFF;

class LLCode
{
  public:
    struct OffsetMapping {
        uint32_t low;
        uint32_t high;
    };

    LLCode(std::unique_ptr<uint8_t[]> bytes, size_t size, uint32_t num_regs,
           uint32_t max_callee_args,
           std::vector<OffsetMapping>&& mappings, BitSet&& gcobj_regs,
           ke::FixedArray<LLBlock>&& blocks)
     : bytes_(std::move(bytes)),
       size_(size),
       num_regs_(num_regs),
       max_callee_args_(max_callee_args),
       mappings_(std::move(mappings)),
       gcobj_regs_(std::move(gcobj_regs)),
       blocks_(std::move(blocks))
    {
        mappings_.shrink_to_fit();
        gcobj_regs_.shrink_to_fit();
    }

    const uint8_t* bytes() const { return bytes_.get(); }
    size_t size() const { return size_; }
    uint32_t num_regs() const { return num_regs_; }
    uint32_t max_callee_args() const { return max_callee_args_; }
    const BitSet& gcobj_regs() const { return gcobj_regs_; }
    const ke::FixedArray<LLBlock>& blocks() const { return blocks_; }

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
    uint32_t max_callee_args_;
    std::vector<OffsetMapping> mappings_;
    BitSet gcobj_regs_;
    ke::FixedArray<LLBlock> blocks_;
};

} // namespace sp::v2
