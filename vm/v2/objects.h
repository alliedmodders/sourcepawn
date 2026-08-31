// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// Copyright (C) 2006-2026 AlliedModders LLC
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

#include <stdint.h>

namespace sp {

class TypeDesc;

namespace v2 {

struct HeapItem {
    const TypeDesc* td;
};

struct SpArray : public HeapItem {
    uint32_t length;
    uint32_t data;
};

struct SpObject : public HeapItem {
};

} // namespace v2
} // namespace sp
