// vim: set sts=2 ts=8 sw=2 tw=99 et:
// 
// Copyright (C) 2016-2018 AlliedModders LLC
// 
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#include "debugging.h"
#include "stack-frames.h"
#include "environment.h"
#include "watchdog_timer.h"
#include <amtl/am-raii.h>

namespace sp {

void InvokeDebugger(PluginContext* ctx, const IErrorReport* report)
{
  // Continue normal execution, if there is no listener registered.
  if (!Environment::get()->debugbreak())
    return;

  if (!ctx->IsDebugging()) {
    ctx->ReportErrorNumber(SP_ERROR_NOTDEBUGGING);
    return;
  }

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
  dbginfo.frm = ctx->frm();

  // Call debug callback.
  Environment::get()->debugbreak()(ctx, dbginfo, report);
}

} // namespace sp
