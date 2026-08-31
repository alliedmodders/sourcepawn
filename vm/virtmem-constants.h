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

#include <amtl/am-bits.h>

// mimalloc organizes its memory arenas into 64KB slices, so we need address
// and size alignment to 64KB.
static constexpr size_t kMimallocSliceSize = 64 * ke::kKB;

// mimalloc's internal chunk alignment within arenas.
static constexpr size_t kMimallocArenaChunkAlignment = 8 * ke::kMB;

// Minimum size of a mimalloc arena.
static constexpr size_t kMinMimallocArenaSize = 32 * ke::kMB;

// We pad allocations to ensure that mimalloc always has enough fully aligned
// 256-slice chunks. This is necessary because the first chunk in an arena uses
// uses 1 slice for metadata, leaving it short for exact chunk requests.
static constexpr size_t kMimallocArenaPadding = 8 * ke::kMB;
