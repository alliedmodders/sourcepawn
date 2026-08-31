// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2012-2026 AlliedModders LLC

#pragma once

#include <unordered_map>

#include "stl-allocator.h"

namespace sp {
namespace cc {
namespace tr {

template <typename Key,
	  typename T,
	  typename Hash = std::hash<Key>,
	  typename KeyEqual = std::equal_to<Key>>
using unordered_map = std::unordered_map<Key, T, Hash, KeyEqual, StlAllocator<std::pair<const Key, T>>>;

} // namespace tr
} // namespace cc
} // namespace sp
