// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2021-2026 AlliedModders LLC
//
#pragma once

#include <string>

#include "stl-allocator.h"

namespace sp {
namespace cc {
namespace tr {

template <typename Char,
	  typename Traits = std::char_traits<Char>>
using basic_string = std::basic_string<Char, Traits, StlAllocator<Char>>;

using string = basic_string<char>;

} // namespace tr
} // namespace cc
} // namespace sp
