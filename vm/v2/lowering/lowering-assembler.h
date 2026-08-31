// vim: set ts=8 sw=4 tw=99 sts=4 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

#include "assembler.h"

namespace sp::v2 {

class LoweringAssembler : public AssemblerBase
{
  public:
    size_t data_size() const override { return 0; }

    template <typename T>
    void emit(const T& val) {
        ensureSpace();
        write<T>(val);
    }

    uint8_t* bytes() const {
        return buffer();
    }
};

} // namespace sp::v2
