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
#ifndef _include_sourcepawn_vm_environment_h_
#define _include_sourcepawn_vm_environment_h_

#include <sp_vm_api.h>
#include <amtl/am-cxx.h>
#include <amtl/am-inlinelist.h>
#include <amtl/am-thread-utils.h>
#include "code-allocator.h"
#include "plugin-runtime.h"
#include "stack-frames.h"

namespace sp {

using namespace SourcePawn;

class PluginRuntime;
class CodeStubs;
class WatchdogTimer;
class ErrorReport;

// An Environment encapsulates everything that's needed to load and run
// instances of plugins on a single thread. There can be at most one
// environment per thread.
//
// Currently, the VM is single threaded in that no more than one
// Environment can be created per process.
class Environment : public ISourcePawnEnvironment
{
 public:
  Environment();
  ~Environment();

  static Environment *New();

  void Shutdown() override;
  ISourcePawnEngine *APIv1() override;
  ISourcePawnEngine2 *APIv2() override;
  int ApiVersion() override {
    return SOURCEPAWN_API_VERSION;
  }

  // Access the current Environment.
  static Environment *get();

  bool InstallWatchdogTimer(int timeout_ms);

  void EnterExceptionHandlingScope(ExceptionHandler *handler) override;
  void LeaveExceptionHandlingScope(ExceptionHandler *handler) override;
  bool HasPendingException(const ExceptionHandler *handler) override;
  const char *GetPendingExceptionMessage(const ExceptionHandler *handler) override;

  // Runtime functions.
  const char *GetErrorString(int err);
  void ReportError(int code);
  void ReportError(int code, const char *message);
  void ReportErrorFmt(int code, const char *message, ...);
  void ReportErrorVA(const char *fmt, va_list ap);
  void ReportErrorVA(int code, const char *fmt, va_list ap);
  void BlamePluginErrorVA(SourcePawn::IPluginFunction *pf, const char *fmt, va_list ap);

  // Allocate and free executable memory.
  CodeChunk AllocateCode(size_t size);

#if defined(SP_HAS_JIT)
  CodeStubs *stubs() {
    return code_stubs_;
  }
#endif

  // Runtime management.
  void RegisterRuntime(PluginRuntime *rt);
  void DeregisterRuntime(PluginRuntime *rt);
  void PatchAllJumpsForTimeout();
  void UnpatchAllJumpsFromTimeout();
  ke::Mutex *lock() {
    return &mutex_;
  }

  bool Invoke(PluginContext* cx, const RefPtr<MethodInfo>& method, cell_t* result);

  // Helpers.
  void SetProfiler(IProfilingTool *profiler) {
    profiler_ = profiler;
  }
  IProfilingTool *profiler() const {
    return profiler_;
  }
  bool IsProfilingEnabled() const {
    return profiling_enabled_;
  }
  void EnableProfiling();
  void DisableProfiling();

  void SetJitEnabled(bool enabled);
  bool IsJitEnabled() const {
    return jit_enabled_;
  }
  void SetDebugger(IDebugListener *debugger) {
    debugger_ = debugger;
  }
  IDebugListener *debugger() const {
    return debugger_;
  }

  WatchdogTimer *watchdog() const {
    return watchdog_timer_;
  }

  bool hasPendingException() const;
  void clearPendingException();
  int getPendingExceptionCode() const;

  // These are indicators used for the watchdog timer.
  uintptr_t FrameId() const {
    return frame_id_;
  }
  bool RunningCode() const {
    return !!top_;
  }

  void enterInvoke(InvokeFrame *frame);
  void leaveJitInvoke(JitInvokeFrame* frame);
  void leaveInvoke();

  InvokeFrame *top() const {
    return top_;
  }
  intptr_t* exit_fp() const {
    return exit_fp_;
  }

 public:
  static inline size_t offsetOfTopFrame() {
    return offsetof(Environment, top_);
  }
  static inline size_t offsetOfExceptionCode() {
    return offsetof(Environment, exception_code_);
  }

  void* addressOfExit() {
    return &exit_fp_;
  }
  void* addressOfExceptionCode() {
    return &exception_code_;
  }

 private:
  bool Initialize();

 private:
  void DispatchReport(const ErrorReport &report);
  ke::AutoPtr<ISourcePawnEngine> api_v1_;
  ke::AutoPtr<ISourcePawnEngine2> api_v2_;
  ke::AutoPtr<WatchdogTimer> watchdog_timer_;
  ke::Mutex mutex_;

  IDebugListener *debugger_;
  ExceptionHandler *eh_top_;
  int exception_code_;
  char exception_message_[1024];

  IProfilingTool *profiler_;
  bool jit_enabled_;
  bool profiling_enabled_;

  ke::AutoPtr<CodeAllocator> code_alloc_;
#if defined(SP_HAS_JIT)
  ke::AutoPtr<CodeStubs> code_stubs_;
#endif

  ke::InlineList<PluginRuntime> runtimes_;

  uintptr_t frame_id_;

  InvokeFrame *top_;
  intptr_t* exit_fp_;
};

class EnterProfileScope
{
 public:
  EnterProfileScope(const char *group, const char *name)
  {
    if (Environment::get()->IsProfilingEnabled())
      Environment::get()->profiler()->EnterScope(group, name);
  }

  ~EnterProfileScope()
  {
    if (Environment::get()->IsProfilingEnabled())
      Environment::get()->profiler()->LeaveScope();
  }
};

class ErrorReport : public SourcePawn::IErrorReport
{
  public:
  ErrorReport(int code, const char *message, PluginContext *cx, SourcePawn::IPluginFunction *pf);
  int Code() const;

  public: //IErrorReport
  const char *Message() const override;
  IPluginFunction *Blame() const override;
  bool IsFatal() const override;
  IPluginContext *Context() const override;

 private:
  int code_;
  const char *message_;
  PluginContext *context_;
  IPluginFunction *blame_;
};

} // namespace sp

#endif // _include_sourcepawn_vm_environment_h_
