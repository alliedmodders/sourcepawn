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
#ifndef _include_sourcepawn_vm_api_h_
#define _include_sourcepawn_vm_api_h_

#include <sp_vm_api.h>
#include <am-cxx.h> // Replace with am-cxx later.

namespace sp {

using namespace SourcePawn;

class SourcePawnEngine : public ISourcePawnEngine
{
 public:
  SourcePawnEngine();

  sp_plugin_t* LoadFromFilePointer(FILE* fp, int* err) override;
  sp_plugin_t* LoadFromMemory(void* base, sp_plugin_t* plugin, int* err) override;
  int FreeFromMemory(sp_plugin_t* plugin) override;
  void* BaseAlloc(size_t size) override;
  void BaseFree(void* memory) override;
  void* ExecAlloc(size_t size) override;
  void ExecFree(void* address) override;
  IDebugListener* SetDebugListener(IDebugListener* pListener) override;
  unsigned int GetContextCallCount() override;
  unsigned int GetEngineAPIVersion() override;
  void* AllocatePageMemory(size_t size) override;
  void SetReadWrite(void* ptr) override;
  void SetReadExecute(void* ptr) override;
  void FreePageMemory(void* ptr) override;
  void SetReadWriteExecute(void* ptr);
  const char* GetErrorString(int err);
  int SetDebugBreakHandler(SPVM_DEBUGBREAK handler) override;
};

class SourcePawnEngine2 : public ISourcePawnEngine2
{
 public:
  SourcePawnEngine2();

  unsigned int GetAPIVersion() override;
  const char* GetEngineName() override;
  const char* GetVersionString() override;
  IPluginRuntime* LoadPlugin(ICompilation* co, const char* file, int* err) override;
  SPVM_NATIVE_FUNC CreateFakeNative(SPVM_FAKENATIVE_FUNC callback, void* pData) override;
  void DestroyFakeNative(SPVM_NATIVE_FUNC func) override;
  IDebugListener* SetDebugListener(IDebugListener* listener) override;
  ICompilation* StartCompilation() override;
  const char* GetErrorString(int err) override;
  bool Initialize() override;
  void Shutdown() override;
  IPluginRuntime* CreateEmptyRuntime(const char* name, uint32_t memory) override;
  bool InstallWatchdogTimer(size_t timeout_ms) override;
  bool SetJitEnabled(bool enabled) override;
  bool IsJitEnabled() override;
  void SetProfiler(IProfiler* profiler) override;
  void EnableProfiling() override;
  void DisableProfiling() override;
  void SetProfilingTool(IProfilingTool* tool) override;
  IPluginRuntime* LoadBinaryFromFile(const char* file, char* error, size_t maxlength) override;
  ISourcePawnEnvironment* Environment() override;

 private:
  char engine_name_[256];
};

extern size_t UTIL_Format(char* buffer, size_t maxlength, const char* fmt, ...);
extern size_t UTIL_FormatVA(char* buffer, size_t maxlength, const char* fmt, va_list ap);

} // namespace sp

#endif // _include_sourcepawn_vm_api_h_
