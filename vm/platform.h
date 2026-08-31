// vim: set sts=4 ts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

#include <stdint.h>

#include <amtl/am-platform.h>

namespace sp {

// Get the end of the stack as an address casted to intptr_t.
intptr_t GetThreadStackLimit();

// Helper function to get the process/kernel page size.
intptr_t GetPageSize();

} // namespace sp
