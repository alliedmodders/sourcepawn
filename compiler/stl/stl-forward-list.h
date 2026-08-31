// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2012-2026 AlliedModders LLC

#pragma once

#include <forward_list>

#include "stl-allocator.h"

namespace sp {
namespace cc {
namespace tr {
 
template <typename T>
using forward_list = std::forward_list<T, StlAllocator<T>>;

} // namespace tr
} // namespace cc
} // namespace sp
