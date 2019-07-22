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
#include "runtime-helpers.h"
#include "environment.h"

namespace sp {

using namespace SourcePawn;

void
ReportOutOfBoundsError(cell_t index, cell_t bounds)
{
  if (bounds == INT_MAX) {
    // This is an internal protection against negative indices on arrays with
    // unknown size.
    Environment::get()->ReportErrorFmt(
      SP_ERROR_ARRAY_BOUNDS,
      "Array index out-of-bounds (index %d)",
      index);
  } else {
    Environment::get()->ReportErrorFmt(
      SP_ERROR_ARRAY_BOUNDS,
      "Array index out-of-bounds (index %d, limit %d)",
      index,
      size_t(bounds) + 1);
  }
}

void
ReportUnboundNative()
{
  Environment::get()->ReportError(SP_ERROR_INVALID_NATIVE);
}

} // namespace sp
