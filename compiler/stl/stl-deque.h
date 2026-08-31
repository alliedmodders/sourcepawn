// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2023-2026 AlliedModders LLC

#pragma once

#include <deque>

#include "stl-allocator.h"

namespace sp {
namespace cc {
namespace tr {

template <typename T>
using deque = std::deque<T, StlAllocator<T>>;

} // namespace tr
} // namespace cc
} // namespace sp
