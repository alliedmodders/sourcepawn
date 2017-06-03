// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2006-2017 AlliedModders LLC
//
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#include <emscripten.h>

#include <sp_vm_api.h>
#include <stdlib.h>
#include <stdarg.h>
#include <am-cxx.h>
#include "dll_exports.h"
#include "environment.h"
#include "stack-frames.h"

using namespace ke;
using namespace sp;
using namespace SourcePawn;

static const char *
BaseFilename(const char *path)
{
  size_t len = strlen(path);
  if (len <= 1)
    return path;
  for (size_t i = len - 2; i < len; i--) {
    if (path[i] == '/' || path[i] == '\\')
      return &path[i + 1];
  }
  return path;
}

static void
DumpStack(IFrameIterator &iter)
{
  int index_count = 0;
  for (; !iter.Done(); iter.Next()) {
    if (iter.IsInternalFrame())
      continue;

    int index = index_count++;

    const char *name = iter.FunctionName();
    if (!name) {
      fprintf(stdout, "  [%d] <unknown>\n", index);
      continue;
    }

    if (iter.IsScriptedFrame()) {
      const char *file = iter.FilePath();
      if (!file)
        file = "<unknown>";
      file = BaseFilename(file);
      fprintf(stdout, "  [%d] %s::%s, line %d\n", index, file, name, iter.LineNumber());
    } else {
      fprintf(stdout, "  [%d] %s()\n", index, name);
    }
  }
}

class ShellDebugListener : public IDebugListener
{
public:
  void ReportError(const IErrorReport &report, IFrameIterator &iter) override {
    fprintf(stdout, "Exception thrown: %s\n", report.Message());
    DumpStack(iter);
  }

  void OnDebugSpew(const char *msg, ...) override {
#if !defined(NDEBUG) && defined(DEBUG)
    va_list ap;
    va_start(ap, msg);
    vfprintf(stderr, msg, ap);
    va_end(ap);
#endif
  }
};

extern "C" EMSCRIPTEN_KEEPALIVE Environment *
new_environment() {
  Environment *env = Environment::New();
  if (!env)
    return nullptr;

  env->SetDebugger(new ShellDebugListener());
  env->InstallWatchdogTimer(5000);

  return env;
}

extern "C" EMSCRIPTEN_KEEPALIVE void
delete_environment(Environment *env) {
  ShellDebugListener *debug = static_cast<ShellDebugListener *>(env->debugger());
  if (debug)
    delete debug;

  env->SetDebugger(nullptr);
  env->Shutdown();

  delete env;
}

extern "C" EMSCRIPTEN_KEEPALIVE IPluginRuntime *
environment_new_runtime(Environment *env, const char *file, char *error, size_t errorLength) {
  IPluginRuntime *rt = env->APIv2()->LoadBinaryFromFile(file, error, errorLength);
  if (!rt)
    return nullptr;

  PluginRuntime *rti = PluginRuntime::FromAPI(rt);
  rti->InstallBuiltinNatives();

  return rt;
}

extern "C" EMSCRIPTEN_KEEPALIVE void
delete_runtime(IPluginRuntime *rt) {
  delete rt;
}

extern "C" EMSCRIPTEN_KEEPALIVE bool
runtime_bind_native(IPluginRuntime *rt, const char *name, SPVM_NATIVE_FUNC fn) {
  int err;
  uint32_t index;
  if ((err = rt->FindNativeByName(name, &index)) != SP_ERROR_NONE)
    return false;

  rt->UpdateNativeBinding(index, fn, 0, nullptr);
  return true;
}

extern "C" EMSCRIPTEN_KEEPALIVE IPluginFunction *
runtime_get_function(IPluginRuntime *rt, const char *name) {
  return rt->GetFunctionByName(name);
}

extern "C" EMSCRIPTEN_KEEPALIVE bool
function_push_cell(IPluginFunction *fun, cell_t cell) {
  return fun->PushCell(cell) == SP_ERROR_NONE;
}

extern "C" EMSCRIPTEN_KEEPALIVE bool
function_push_float(IPluginFunction *fun, float number) {
  return fun->PushFloat(number) == SP_ERROR_NONE;
}

extern "C" EMSCRIPTEN_KEEPALIVE bool
function_push_string(IPluginFunction *fun, const char *buffer) {
  return fun->PushString(buffer) == SP_ERROR_NONE;
}

extern "C" EMSCRIPTEN_KEEPALIVE bool
function_invoke(IPluginFunction *fun, cell_t *retval, char *error, size_t errorLength) {
  IPluginRuntime *rt = fun->GetParentRuntime();
  IPluginContext *cx = rt->GetDefaultContext();

  {
    ExceptionHandler eh(cx);
    if (!fun->Invoke(retval)) {
      SafeStrcpy(error, errorLength, eh.Message());
      return false;
    }
  }

  return true;
}

extern "C" EMSCRIPTEN_KEEPALIVE cell_t *
context_local_to_physical_address(IPluginContext *ctx, cell_t local) {
  cell_t *physical;
  if (ctx->LocalToPhysAddr(local, &physical) != SP_ERROR_NONE) {
    return nullptr;
  }
  return physical;
}

extern "C" EMSCRIPTEN_KEEPALIVE void
context_throw_native_error(IPluginContext *ctx, const char *error) {
  ctx->ThrowNativeError(error);
}
