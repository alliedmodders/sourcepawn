// vim: set sts=4 ts=4 sw=4 tw=99 et:
//
// Copyright (C) 2026 AlliedModders LLC
//
// This file is part of SourcePawn.
//
// SourcePawn is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the
// Free Software Foundation, either version 3 of the License, or (at your
// option) any later version.
//
// SourcePawn is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License along
// with SourcePawn.  If not, see <http://www.gnu.org/licenses/>.
#pragma once

#include <stdint.h>

#include <amtl/am-platform.h>

namespace sp {

// Get the end of the stack as an address casted to intptr_t.
intptr_t GetThreadStackLimit();

// Helper function to get the process/kernel page size.
intptr_t GetPageSize();

} // namespace sp
