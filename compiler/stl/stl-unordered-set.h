// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2012-2026 AlliedModders LLC

#pragma once

#include <unordered_set>

#include "stl-allocator.h"

namespace sp {
namespace cc {
namespace tr {

template <typename Key,
	  typename Hash = std::hash<Key>,
	  typename KeyEqual = std::equal_to<Key>>
using unordered_set = std::unordered_set<Key, Hash, KeyEqual, StlAllocator<Key>>;

} // namespace tr
} // namespace cc
} // namespace sp
