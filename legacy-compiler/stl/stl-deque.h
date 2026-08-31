// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// Copyright (C) 2023 AlliedModders LLC
//
// SourcePawn is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option)
// any later version.
//
// SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.

#pragma once

#include <deque>

#include "stl-allocator.h"

namespace sp {
namespace cc {
namespace tr {

template <typename T>
using deque = std::deque<T, StlAllocator<T>>;

} // namespace tr
} // namespace cc
} // namespace sp
