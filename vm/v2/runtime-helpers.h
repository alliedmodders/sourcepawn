// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

#include <sp_vm_types.h>

namespace sp::v2 {

class Runtime;
struct NativeEntry;

cell_t NativeInvokeThunk(Runtime* ctx, NativeEntry* entry, const cell_t* params);
int Int64Div(int64_t* pri, int64_t* alt, int64_t* pri_dest);
int Int64Mod(int64_t* pri, int64_t* alt, int64_t* pri_dest);
double DoubleMod(double left, double right);
void ReportOutOfBoundsError(cell_t index, cell_t bounds);

} // namespace sp::v2
