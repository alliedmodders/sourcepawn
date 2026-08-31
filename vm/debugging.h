// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#ifndef _include_sourcepawn_vm_debugging_h_
#define _include_sourcepawn_vm_debugging_h_

namespace SourcePawn {
class IErrorReport;
typedef class IPluginRuntime IPluginContext;
} // namepsace SourcePawn

namespace sp {

int InvokeDebugger(SourcePawn::IPluginContext* ctx, const SourcePawn::IErrorReport* report);

} // namespace sp

#endif // _include_sourcepawn_vm_debugging_h_
