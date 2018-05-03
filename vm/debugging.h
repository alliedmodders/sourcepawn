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
//
#ifndef _include_sourcepawn_vm_debugging_h_
#define _include_sourcepawn_vm_debugging_h_

#include "plugin-context.h"

namespace sp {

void InvokeDebugger(PluginContext* ctx, const IErrorReport* report);

} // namespace sp

#endif // _include_sourcepawn_vm_debugging_h_