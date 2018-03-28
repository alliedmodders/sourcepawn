// vim: set sts=2 ts=8 sw=2 tw=99 et:
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
#ifndef _include_sourcepawn_vm_console_debugger_h_
#define _include_sourcepawn_vm_console_debugger_h_

#include "sp_vm_api.h"
#include "sp_vm_debug_api.h"
#include "plugin-context.h"
#include <smx/smx-headers.h>
#include <smx/smx-v1.h>
#include "smx-v1-image.h"
#include "stack-frames.h"

namespace sp {

using namespace SourcePawn;

int InvokeDebugger(PluginContext *ctx);

static const uint32_t MAXLINELENGTH = 128;

enum Runmode {
  STEPPING, /* step into functions */
  STEPOVER, /* step over functions */
  STEPOUT, /* run until the function returns */
  RUNNING, /* just run */
};

class Breakpoint;

class Debugger {
public:
  Debugger(PluginContext *context);
  bool Initialize();
  bool active();
  void Activate();
  void Deactivate();

  void ReportError(const IErrorReport &report, FrameIterator &iter);

public:
  Breakpoint *AddBreakpoint(const char *file, unsigned int line, bool temporary);
  Breakpoint *AddBreakpoint(const char *file, const char *function, bool temporary);
  bool ClearBreakpoint(int number);
  bool ClearBreakpoint(Breakpoint *);
  void ClearAllBreakpoints();
  bool CheckBreakpoint(cell_t cip);
  int FindBreakpoint(char *breakpoint);
  void ListBreakpoints();
  char *ParseBreakpointLine(char *input, const char **filename);

  bool AddWatch(const char *symname);
  bool ClearWatch(const char *symname);
  bool ClearWatch(uint32_t num);
  void ClearAllWatches();
  void ListWatches();

  bool GetSymbolValue(const SmxV1Image::Symbol *sym, int index, cell_t *value);
  bool SetSymbolValue(const SmxV1Image::Symbol *sym, int index, cell_t value);
  bool SetSymbolString(const SmxV1Image::Symbol* sym, char* str);
  char *GetString(SmxV1Image::Symbol *sym);
  void PrintValue(long value, int disptype);
  void DisplayVariable(SmxV1Image::Symbol *sym, uint32_t index[], int idxlevel);
  void DumpStack();

public:
  Runmode runmode();
  void SetRunmode(Runmode runmode);
  cell_t lastframe();
  void SetLastFrame(cell_t lastfrm);
  uint32_t lastline();
  void SetLastLine(uint32_t line);
  uint32_t breakcount();
  void SetBreakCount(uint32_t breakcount);
  const char *currentfile();
  void SetCurrentFile(const char *file);

public:
  void HandleInput(cell_t cip, bool isBp);
  void ListCommands(const char *command);

  // String/Path helpers
  static const char *SkipPath(const char *str);
  static char *SkipWhitespace(char *str);
  static char *TrimString(char *string);

private:
  void HandleHelpCmd(const char *line);
  void HandleQuitCmd();
  bool HandleGoCmd(char *params);
  void HandleFunctionListCmd();
  void HandleFrameCmd(char *params);
  void HandleBreakpointCmd(char *command, char *params);
  void HandleClearBreakpointCmd(char *params);
  void HandleVariableDisplayCmd(char *params);
  void HandleSetVariableCmd(char *params);
  void HandleFilesListCmd();
  void HandleDisplayFormatChangeCmd(char *params);
  void HandlePrintPositionCmd();
  void HandleWatchCmd(char *params);
  void HandleClearWatchCmd(char *params);
  void HandleDumpMemoryCmd(char *command, char *params);

public:
  struct BreakpointMapPolicy {

    static inline uint32_t hash(ucell_t value) {
      return ke::HashInteger<4>(value);
    }

    static inline bool matches(ucell_t a, ucell_t b) {
      return a == b;
    }
  };
  typedef ke::HashMap<ucell_t, Breakpoint *, BreakpointMapPolicy> BreakpointMap;

  BreakpointMap breakpoint_map_;

private:
  PluginContext *context_;
  Runmode runmode_;
  cell_t lastfrm_;
  uint32_t lastline_;
  uint32_t breakcount_;
  const char *currentfile_;
  bool active_;

  // Temporary variables to use inside command loop
  cell_t cip_;
  cell_t frm_;
  FrameIterator *frames_;
  uint32_t frame_count_;
  uint32_t selected_frame_;
  PluginContext *selected_context_;

  

  struct WatchTablePolicy {
    typedef AString Payload;

    static uint32_t hash(const char *str) {
      return ke::HashCharSequence(str, strlen(str));
    }

    static bool matches(const char *key, Payload str) {
      return str.compare(key) == 0;
    }
  };
  typedef ke::HashTable<WatchTablePolicy> WatchTable;

  WatchTable watch_table_;
};

class Breakpoint : IBreakpoint {
public:
  Breakpoint(LegacyImage *image, ucell_t addr, const char *name, bool temporary = false)
    : image_(image),
    addr_(addr),
    name_(name),
    temporary_(temporary)
  {}

  ucell_t addr() {
    return addr_;
  }
  const char *name() override {
    return name_;
  }
  bool temporary() override {
    return temporary_;
  }
  const char *filename() override {
    return Debugger::SkipPath(image_->LookupFile(addr_));
  }
  uint32_t line() override {
    uint32_t line;
    if (image_->LookupLine(addr_, &line))
      return line;
    return 0;
  }
private:
  LegacyImage *image_; /* file image of plugin the address is in */
  ucell_t addr_; /* address (in code or data segment) */
  const char *name_; /* name of the symbol (function) */
  bool temporary_; /* delete breakpoint when hit? */
};

} // namespace sp

namespace SourcePawn {
  class ConsoleDebugger : IConsoleDebugger {
  public:
    ConsoleDebugger() 
      : enabled_(false),
        debugNext_(false)
    {}

  public: //IConsoleDebugger
    int ApiVersion() override;
    bool IsEnabled() override;
    bool SetEnabled(bool enable) override;
    bool DebugNextLoadedPlugin() override;
    bool StartDebugger(const IPluginContext *ctx) override;
    ke::Vector<IBreakpoint *> *GetBreakpoints(const IPluginContext *ctx) override;
    IBreakpoint *AddBreakpoint(const IPluginContext *ctx, const char *line, bool temporary) override;
    bool ClearBreakpoint(const IPluginContext *ctx, int bpnum) override;

  public:
    bool ShouldDebugNextLoadedPlugin();
    void ResetDebugNextLoadedPlugin();
  private:
    bool enabled_;
    bool debugNext_;
  };
}
#endif  /* _include_sourcepawn_vm_console_debugger_h_ */

