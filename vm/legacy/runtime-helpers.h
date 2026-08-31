// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#ifndef _include_sourcepawn_runtime_helpers_h_
#define _include_sourcepawn_runtime_helpers_h_

#include <sp_vm_types.h>

namespace sp::v1 {

class PluginRuntime;
typedef PluginRuntime PluginContext;
struct NativeEntry;

cell_t NativeInvokeThunk(PluginContext* ctx, NativeEntry* entry, const cell_t* params);
int Int64Div(int64_t* pri, int64_t* alt, int64_t* pri_dest);
int Int64Mod(int64_t* pri, int64_t* alt, int64_t* pri_dest);
void ReportOutOfBoundsError(cell_t index, cell_t bounds);

} // namespace sp::v1

#endif // _include_sourcepawn_runtime_helpers_h_
