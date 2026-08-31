// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

#include <stddef.h>
#include <stdint.h>

#include <optional>

namespace sp {

// Finds the next unmapped memory region of the requested size.
// Returns 0 if no suitable region is found.
// `start` is the minimum address to consider.
// `end` is the maximum address (the region must end at or before `end`).
std::optional<uintptr_t> FindNextMmapCandidate(uintptr_t start, size_t size, uintptr_t end);

} // namespace sp
