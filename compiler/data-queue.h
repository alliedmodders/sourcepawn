// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
// Copyright (c) ITB CompuPhase, 1997-2006
//
#pragma once

#include <string>
#include <vector>
#include "sc.h"
#include "symbols.h"

namespace sp {
namespace cc {

class DataQueue final
{
  public:
    DataQueue();

    void Add(cell value);
    void Add(tr::vector<cell>&& cells);
    void Add(const char* text, size_t length);
    void AddZeroes(cell count);

    void Add(const std::string& str) {
        Add(str.data(), str.size());
    }

    cell size() const { return (cell)buffer_.size(); }
    cell dat_address() const { return (cell)buffer_.size(); }
    const uint8_t* dat() const { return reinterpret_cast<const uint8_t*>(buffer_.data()); }

  private:
    std::string buffer_;
};

} // namespace cc
} // namespace sp
