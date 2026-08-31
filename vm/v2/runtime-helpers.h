// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
// This file is part of SourcePawn.
//
// SourcePawn is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// SourcePawn is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with SourcePawn.  If not, see <http://www.gnu.org/licenses/>.
#pragma once

#include <sp_vm_types.h>

namespace sp {
struct HeapItem;
} // namespace sp

namespace sp::v2 {

class Runtime;
struct NativeEntry;

cell_t NativeInvokeThunk(Runtime* ctx, NativeEntry* entry, const cell_t* params);
int Int64Div(int64_t* pri, int64_t* alt, int64_t* pri_dest);
int Int64Mod(int64_t* pri, int64_t* alt, int64_t* pri_dest);
void ReportOutOfBoundsError(cell_t index, cell_t bounds);

} // namespace sp::v2
