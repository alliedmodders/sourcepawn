// vim: set ts=4 sw=4 tw=99 noet:
// 
// Copyright (C) 2006-2015 AlliedModders LLC
// 
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#ifndef _INCLUDE_SOURCEPAWN_VM_DEBUG_API_H_
#define _INCLUDE_SOURCEPAWN_VM_DEBUG_API_H_

/**
* @file sp_vm_debug_api.h
* @brief Contains all of the object structures used to interface with the experimental sourcepawn console debugger.
*/

#include <am-vector.h>

namespace SourcePawn
{
  class IPluginContext;

  // @brief Represents a debug breakpoint in a plugin.
  class IBreakpoint
  {
  public:

    // @brief Returns the name of the symbol (function) the breakpoint was set on if any.
    virtual const char *name() = 0;

    // @brief Returns the name of the file in which the line of the breakpoint is.
    virtual const char *filename() = 0;
    
    // @brief Returns the line in the source file of the breakpoint.
    virtual uint32_t line() = 0;

    // @brief Returns whether the breakpoint is removed when it's hit or not.
    virtual bool temporary() = 0;
  };

  // @brief Interface to the experimental console debugger.
  // API is not stable yet.
  class IConsoleDebugger
  {
  public:

    // @brief Returns whether debugging is enabled in general.
    virtual bool IsEnabled() = 0;

    /** @brief Enables debugging in general. Prepares generated code for debugging.
     * Must be set before a plugin is loaded.
     *
     * @param enable  Allow debugging?
     * @return        True if debugging is allowed now, false if there are loaded plugins already.
     */
    virtual bool SetEnabled(bool enable) = 0;

    /**
     * @brief Activates the debugger on a plugin. Plugin will pause on the next instruction.
     * @param ctx  Context of the plugin to debug.
     * @return     True if debugging started, false otherwise.
     */
    virtual bool StartDebugger(const IPluginContext *ctx) = 0;

    /**
     * @brief Returns a list of all active breakpoints in a plugin.
     * @param ctx  Context of the plugin.
     * @return     List of breakpoints.
     */
    virtual ke::Vector<IBreakpoint *> *GetBreakpoints(const IPluginContext *ctx) = 0;

    /**
     * @brief Adds a breakpoint at a line or function in a plugin.
     * Can be of format "<file>:<line>" or "<file>:<function>".
     *
     * @param ctx  Context of the plugin.
     * @param line String describing the address to break on.
     * @return     The new breakpoint or nullptr on error.
     */
    virtual IBreakpoint *AddBreakpoint(const IPluginContext *ctx, const char *line, bool temporary) = 0;

    /**
     * @brief Removes a breakpoint from a plugin.
     * @param ctx   Context of the plugin.
     * @param bpnum Breakpoint number to remove.
     * @return      True if breakpoint found and removed, false otherwise.
     */
    virtual bool ClearBreakpoint(const IPluginContext *ctx, int bpnum) = 0;
  };

  // @brief A function named "GetConsoleDebugger" is exported from the
  // SourcePawn DLL, conforming to the following signature:
  typedef IConsoleDebugger *(*GetConsoleDebuggerFn)();
}
#endif //_INCLUDE_SOURCEPAWN_VM_DEBUG_API_H_
