// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2016-2026 AlliedModders LLC
//
#include "debugging.h"
#include <amtl/am-raii.h>
#include "environment.h"
#include "stack-frames.h"
#include "watchdog_timer.h"

namespace sp {

using namespace SourcePawn;

int InvokeDebugger(IPluginContext* ctx, const IErrorReport* report) {
    // Continue normal execution, if there is no listener registered.
    if (!Environment::get()->debugbreak())
        return SP_ERROR_NONE;

    if (!ctx->IsDebugging())
        return SP_ERROR_NOTDEBUGGING;

    cell_t cip = 0;

    // Find first scripted frame on the stack to get the cip from.
    // There might be some native or helper frames beforehand.
    {
        FrameIterator iter;
        for (; !iter.Done(); iter.Next()) {
            if (iter.IsScriptedFrame()) {
                cip = iter.cip();
                break;
            }
        }
    }

    // Tell the watchdog to take a break.
    // We might stay in the debugger callback for a while,
    // so don't let the watchdog hit immediately after
    // continueing with execution.
    ke::SaveAndSet<bool> disableWatchdog(&Environment::get()->watchdog()->ignore_timeout_, true);

    // Fill in the debug info struct.
    sp_debug_break_info_t dbginfo;
    dbginfo.version = DEBUG_BREAK_INFO_VERSION;
    dbginfo.cip = cip;
    dbginfo.frm = 0;

    // Call debug callback.
    Environment::get()->debugbreak()(ctx, dbginfo, report);
    return SP_ERROR_NONE;
}

} // namespace sp
