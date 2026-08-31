// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
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
