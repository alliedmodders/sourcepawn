// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

#include <assert.h>
#include <stddef.h>
#include <stdint.h>

#include <amtl/am-bits.h>

namespace sp {

class VirtMem32 {
  public:
    bool Initialize();

    uint32_t ToLocalAddr(void* p) {
        static_assert(sizeof(uint32_t) == sizeof(uintptr_t));
        return reinterpret_cast<uint32_t>(p);
    }
    template <typename T>
    T ToPhysAddr(uint32_t addr) {
        return reinterpret_cast<T>(addr);
    }
};

} // namespace sp
