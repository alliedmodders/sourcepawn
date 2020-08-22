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
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include "environment.h"
#include "api.h"
#include <zlib/zlib.h>
#if defined __GNUC__
#include <unistd.h>
#endif

#if defined WIN32
 #define WIN32_LEAN_AND_MEAN
 #include <windows.h>
#elif defined __GNUC__
 #include <sys/mman.h>
#endif

#if defined __linux__
#include <malloc.h>
#endif

#if defined(SOURCEMOD_BUILD)
# include <sourcemod_version.h>
# define SOURCEPAWN_VERSION SOURCEMOD_VERSION
#endif
#include "code-stubs.h"
#include "smx-v1-image.h"
#include <amtl/am-string.h>

using namespace sp;
using namespace SourcePawn;

// ////// //
// API v1
// ////// //

SourcePawnEngine::SourcePawnEngine()
{
}

const char*
SourcePawnEngine::GetErrorString(int error)
{
  return Environment::get()->GetErrorString(error);
}

void*
SourcePawnEngine::ExecAlloc(size_t size)
{
#if defined WIN32
  return VirtualAlloc(NULL, size, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
#elif defined __GNUC__
# if defined __APPLE__
  void* base = valloc(size);
# else
  void* base = memalign(sysconf(_SC_PAGESIZE), size);
# endif
  if (mprotect(base, size, PROT_READ|PROT_WRITE|PROT_EXEC) != 0) {
    free(base);
    return NULL;
  }
  return base;
#endif
}

void*
SourcePawnEngine::AllocatePageMemory(size_t size)
{
  CodeChunk chunk = Environment::get()->AllocateCode(size + sizeof(CodeChunk));
  CodeChunk* hidden = (CodeChunk*)chunk.address();
  new (hidden) CodeChunk(chunk);
  return hidden + 1;
}

void
SourcePawnEngine::SetReadExecute(void* ptr)
{
  /* already re */
}

void
SourcePawnEngine::SetReadWrite(void* ptr)
{
  /* already rw */
}

void
SourcePawnEngine::FreePageMemory(void* ptr)
{
  assert(ptr);
  CodeChunk* hidden = (CodeChunk*)((uint8_t*)ptr - sizeof(CodeChunk));
  hidden->~CodeChunk();
}

void
SourcePawnEngine::ExecFree(void* address)
{
#if defined WIN32
  VirtualFree(address, 0, MEM_RELEASE);
#elif defined __GNUC__
  free(address);
#endif
}

void
SourcePawnEngine::SetReadWriteExecute(void* ptr)
{
  //:TODO:  g_ExeMemory.SetRWE(ptr);
  SetReadExecute(ptr);
}

void*
SourcePawnEngine::BaseAlloc(size_t size)
{
  return malloc(size);
}

void
SourcePawnEngine::BaseFree(void* memory)
{
  free(memory);
}

sp_plugin_t*
SourcePawnEngine::LoadFromFilePointer(FILE* fp, int* err)
{
  if (err != NULL)
    *err = SP_ERROR_ABORTED;

  return NULL;
}

sp_plugin_t*
SourcePawnEngine::LoadFromMemory(void* base, sp_plugin_t* plugin, int* err)
{
  if (err != NULL)
    *err = SP_ERROR_ABORTED;

  return NULL;
}

int
SourcePawnEngine::FreeFromMemory(sp_plugin_t* plugin)
{
  return SP_ERROR_ABORTED;
}

IDebugListener*
SourcePawnEngine::SetDebugListener(IDebugListener* pListener)
{
  IDebugListener* old = Environment::get()->debugger();
  Environment::get()->SetDebugger(pListener);
  return old;
}

int
SourcePawnEngine::SetDebugBreakHandler(SPVM_DEBUGBREAK handler)
{
  if (!Environment::get()->IsDebugBreakEnabled())
    return SP_ERROR_NOTDEBUGGING;

  Environment::get()->SetDebugBreakHandler(handler);
  return SP_ERROR_NONE;
}

unsigned int
SourcePawnEngine::GetEngineAPIVersion()
{
  return 5;
}

unsigned int
SourcePawnEngine::GetContextCallCount()
{
  return 0;
}

// ////// //
// API v2
// ////// //

SourcePawnEngine2::SourcePawnEngine2()
{
}

size_t
sp::UTIL_FormatVA(char* buffer, size_t maxlength, const char* fmt, va_list ap)
{
  size_t len = vsnprintf(buffer, maxlength, fmt, ap);

  if (len >= maxlength) {
    buffer[maxlength - 1] = '\0';
    return maxlength - 1;
  }
  return len;
}

size_t
sp::UTIL_Format(char* buffer, size_t maxlength, const char* fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  size_t len = UTIL_FormatVA(buffer, maxlength, fmt, ap);
  va_end(ap);

  return len;
}

IPluginRuntime*
SourcePawnEngine2::LoadPlugin(ICompilation* co, const char* file, int* err)
{
  if (co) {
    if (err)
      *err = SP_ERROR_PARAM;
    return nullptr;
  }

  IPluginRuntime* rt = LoadBinaryFromFile(file, nullptr, 0);
  if (!rt) {
    if (err) {
      if (FILE* fp = fopen(file, "rb")) {
        fclose(fp);
        *err = SP_ERROR_FILE_FORMAT;
      } else {
        *err = SP_ERROR_NOT_FOUND;
      }
    }
    return nullptr;
  }

  return rt;
}

IPluginRuntime*
SourcePawnEngine2::LoadBinaryFromFile(const char* file, char* error, size_t maxlength)
{
  FILE* fp = fopen(file, "rb");

  if (!fp) {
    UTIL_Format(error, maxlength, "file not found");
    return nullptr;
  }

  std::unique_ptr<SmxV1Image> image(new SmxV1Image(fp));
  fclose(fp);

  if (!image->validate()) {
    const char* errorMessage = image->errorMessage();
    if (!errorMessage)
      errorMessage = "file parse error";
    UTIL_Format(error, maxlength, "%s", errorMessage);
    return nullptr;
  }

  PluginRuntime* pRuntime = new PluginRuntime(image.release());
  if (!pRuntime->Initialize()) {
    delete pRuntime;

    UTIL_Format(error, maxlength, "out of memory");
    return nullptr;
  }

  size_t len = strlen(file);
  for (size_t i = len - 1; i < len; i--) {
    if (file[i] == '/' 
# if defined WIN32
      || file[i] == '\\'
# endif
    )
    {
      pRuntime->SetNames(file, &file[i + 1]);
      break;
    }
  }

  if (!pRuntime->Name())
    pRuntime->SetNames(file, file);

  return pRuntime;
}

SPVM_NATIVE_FUNC
SourcePawnEngine2::CreateFakeNative(SPVM_FAKENATIVE_FUNC, void*)
{
  return nullptr;
}

void
SourcePawnEngine2::DestroyFakeNative(SPVM_NATIVE_FUNC)
{
}

#if !defined(SOURCEPAWN_VERSION)
# define SOURCEPAWN_VERSION "SourcePawn 1.10"
#endif

const char*
SourcePawnEngine2::GetEngineName()
{
  const char* info = "";
#if !defined(SP_HAS_JIT)
  info = ", interp-x86";
#else
  if (!Environment::get()->IsJitEnabled()) {
    info = ", interp-x86";
  } else {
# if defined(KE_ARCH_X86)
    info = ", jit-x86";
# else
    info = ", unknown";
# endif
  }
#endif

  // This is not pretty, but the API is per-thread, and this only breaks if
  // the caller observes the engine name, without requerying it, after
  // changing the JIT status. We do this because the SourceMod build exposes
  // the version as an extern, not a string literal.
  ke::SafeSprintf(engine_name_, sizeof(engine_name_), "%s%s",
    SOURCEPAWN_VERSION, info);
  return engine_name_;
}

const char*
SourcePawnEngine2::GetVersionString()
{
  return SOURCEPAWN_VERSION;
}

IDebugListener*
SourcePawnEngine2::SetDebugListener(IDebugListener* listener)
{
  IDebugListener* old = Environment::get()->debugger();
  Environment::get()->SetDebugger(listener);
  return old;
}

unsigned int
SourcePawnEngine2::GetAPIVersion()
{
  return SOURCEPAWN_ENGINE2_API_VERSION;
}

ICompilation*
SourcePawnEngine2::StartCompilation()
{
  return nullptr;
}

const char*
SourcePawnEngine2::GetErrorString(int err)
{
  return Environment::get()->GetErrorString(err);
}

bool
SourcePawnEngine2::Initialize()
{
  return true;
}

void
SourcePawnEngine2::Shutdown()
{
}

IPluginRuntime*
SourcePawnEngine2::CreateEmptyRuntime(const char* name, uint32_t memory)
{
  std::unique_ptr<EmptyImage> image(new EmptyImage(memory));

  PluginRuntime* rt = new PluginRuntime(image.release());
  if (!rt->Initialize()) {
    delete rt;
    return NULL;
  }

  if (!name)
    name = "<anonymous>";
  rt->SetNames(name, name);
  return rt;
}

bool
SourcePawnEngine2::InstallWatchdogTimer(size_t timeout_ms)
{
  return Environment::get()->InstallWatchdogTimer(timeout_ms);
}

bool
SourcePawnEngine2::SetJitEnabled(bool enabled)
{
  Environment::get()->SetJitEnabled(enabled);
  return Environment::get()->IsJitEnabled() == enabled;
}

bool
SourcePawnEngine2::IsJitEnabled()
{
  return Environment::get()->IsJitEnabled();
}

void
SourcePawnEngine2::SetProfiler(IProfiler* profiler)
{
  // Deprecated.
}

void
SourcePawnEngine2::EnableProfiling()
{
  Environment::get()->EnableProfiling();
}

void
SourcePawnEngine2::DisableProfiling()
{
  Environment::get()->DisableProfiling();
}

void
SourcePawnEngine2::SetProfilingTool(IProfilingTool* tool)
{
  Environment::get()->SetProfiler(tool);
}

ISourcePawnEnvironment*
SourcePawnEngine2::Environment()
{
  return Environment::get();
}
