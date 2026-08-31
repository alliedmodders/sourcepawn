// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// Copyright (C) 2026 AlliedModders LLC
//
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
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
