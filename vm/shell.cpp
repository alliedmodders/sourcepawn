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
#include <sp_vm_api.h>
#include <stdlib.h>
#include <stdarg.h>
#include <amtl/am-cxx.h>
#include <amtl/experimental/am-argparser.h>
#include "dll_exports.h"
#include "environment.h"
#include "stack-frames.h"

#ifdef __EMSCRIPTEN__
# include <emscripten.h>
#endif

#if defined(SOURCEMOD_BUILD)
# include <sourcemod_version.h>
# define SOURCEPAWN_VERSION SOURCEMOD_VERSION
#endif

using namespace ke;
using namespace ke::args;
using namespace sp;
using namespace SourcePawn;

Environment* sEnv;

static const char*
BaseFilename(const char* path)
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
DumpStack(IFrameIterator& iter)
{
  int index_count = 0;
  for (; !iter.Done(); iter.Next()) {
    if (iter.IsInternalFrame())
      continue;

    int index = index_count++;

    const char* name = iter.FunctionName();
    if (!name) {
      fprintf(stdout, "  [%d] <unknown>\n", index);
      continue;
    }

    if (iter.IsScriptedFrame()) {
      const char* file = iter.FilePath();
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
  void ReportError(const IErrorReport& report, IFrameIterator& iter) override {
    fprintf(stdout, "Exception thrown: %s\n", report.Message());
    DumpStack(iter);
  }

  void OnDebugSpew(const char* msg, ...) override {
#if !defined(NDEBUG) && defined(DEBUG)
    va_list ap;
    va_start(ap, msg);
    vfprintf(stderr, msg, ap);
    va_end(ap);
#endif
  }
};

static cell_t Print(IPluginContext* cx, const cell_t* params)
{
  char* p;
  cx->LocalToString(params[1], &p);

  return printf("%s", p);
}

static cell_t WriteNum(IPluginContext* cx, const cell_t* params)
{
  return printf("%d", params[1]);
}

static cell_t PrintNum(IPluginContext* cx, const cell_t* params)
{
  return printf("%d\n", params[1]);
}

static cell_t PrintNums(IPluginContext* cx, const cell_t* params)
{
  for (size_t i = 1; i <= size_t(params[0]); i++) {
    int err;
    cell_t* addr;
    if ((err = cx->LocalToPhysAddr(params[i], &addr)) != SP_ERROR_NONE)
      return cx->ThrowNativeErrorEx(err, "Could not read argument");
    fprintf(stdout, "%d", *addr);
    if (i != size_t(params[0]))
      fprintf(stdout, ", ");
  }
  fprintf(stdout, "\n");
  return 1;
}

static cell_t DoNothing(IPluginContext* cx, const cell_t* params)
{
  return 1;
}

static void BindNative(IPluginRuntime* rt, const char* name, SPVM_NATIVE_FUNC fn)
{
  int err;
  uint32_t index;
  if ((err = rt->FindNativeByName(name, &index)) != SP_ERROR_NONE)
    return;

  rt->UpdateNativeBinding(index, fn, 0, nullptr);
}

static void BindNative(IPluginRuntime* rt, const char* name, INativeCallback* callback)
{
  int err;
  uint32_t index;
  if ((err = rt->FindNativeByName(name, &index)) != SP_ERROR_NONE)
    return;

  rt->UpdateNativeBindingObject(index, callback, 0, nullptr);
}

static cell_t PrintFloat(IPluginContext* cx, const cell_t* params)
{
  return printf("%f\n", sp_ctof(params[1]));
}

static cell_t WriteFloat(IPluginContext* cx, const cell_t* params)
{
  return printf("%f", sp_ctof(params[1]));
}

static cell_t DoExecute(IPluginContext* cx, const cell_t* params)
{
  int32_t ok = 0;
  for (size_t i = 0; i < size_t(params[2]); i++) {
    if (IPluginFunction* fn = cx->GetFunctionById(params[1])) {
      if (fn->Execute(nullptr) != SP_ERROR_NONE)
        continue;
      ok++;
    }
  }
  return ok;
}

static cell_t DoInvoke(IPluginContext* cx, const cell_t* params)
{
  for (size_t i = 0; i < size_t(params[2]); i++) {
    if (IPluginFunction* fn = cx->GetFunctionById(params[1])) {
      if (!fn->Invoke())
        return 0;
    }
  }
  return 1;
}

static cell_t DumpStackTrace(IPluginContext* cx, const cell_t* params)
{
  FrameIterator iter;
  DumpStack(iter);
  return 0;
}

static cell_t ReportError(IPluginContext* cx, const cell_t* params)
{
  cx->ReportError("What the crab?!");
  return 0;
}

class DynamicNative : public INativeCallback
{
  public:
    explicit DynamicNative()
    {}

    void AddRef() override {
      refcount_++;
    }
    void Release() override {
      assert(refcount_ > 0);
      if (--refcount_ == 0)
        delete this;
    }
    int Invoke(IPluginContext* cx, const cell_t* params) override {
      if (params[0] != 1) {
        cx->ReportError("wrong param count");
        return 0;
      }
      return params[1];
    }

  private:
    uintptr_t refcount_ = 0;
};

static int Execute(const char* file)
{
  char error[255];
  std::unique_ptr<IPluginRuntime> rtb(sEnv->APIv2()->LoadBinaryFromFile(file, error, sizeof(error)));
  if (!rtb) {
    fprintf(stderr, "Could not load plugin %s: %s\n", file, error);
    return 1;
  }

  PluginRuntime* rt = PluginRuntime::FromAPI(rtb.get());

  rt->InstallBuiltinNatives();
  BindNative(rt, "print", Print);
  BindNative(rt, "printnum", PrintNum);
  BindNative(rt, "writenum", WriteNum);
  BindNative(rt, "printnums", PrintNums);
  BindNative(rt, "printfloat", PrintFloat);
  BindNative(rt, "writefloat", WriteFloat);
  BindNative(rt, "donothing", DoNothing);
  BindNative(rt, "execute", DoExecute);
  BindNative(rt, "invoke", DoInvoke);
  BindNative(rt, "dump_stack_trace", DumpStackTrace);
  BindNative(rt, "report_error", ReportError);
  BindNative(rt, "CloseHandle", DoNothing);
  BindNative(rt, "dynamic_native", new DynamicNative());

  IPluginFunction* fun = rt->GetFunctionByName("main");
  if (!fun)
    return 0;

  IPluginContext* cx = rt->GetDefaultContext();

  int result;
  {
    ExceptionHandler eh(cx);
    if (!fun->Invoke(&result)) {
      fprintf(stderr, "Error executing main: %s\n", eh.Message());
      return 1;
    }
  }

  return result;
}

int main(int argc, char** argv)
{
#ifdef __EMSCRIPTEN__
  EM_ASM(
    if (ENVIRONMENT_IS_NODE) {
      FS.mkdir('/fakeroot');
      FS.mount(NODEFS, { root: '/' }, '/fakeroot');
      FS.chdir('/fakeroot/' + process.cwd());
    }
  );
#endif

  Parser parser("SourcePawn standalone shell.");

  ToggleOption disable_jit(parser,
    "i", "disable-jit",
    Some(false),
    "Disable the just-in-time compiler.");
  ToggleOption disable_watchdog(parser,
    "w", "disable-watchdog",
    Some(false),
    "Disable the watchdog timer.");
  StringOption filename(parser,
    "file",
    "SMX file to execute.");
  StopOption show_version(parser,
    "-v", "--version",
    Some(false),
    "Print version information and exit.");

  if (!parser.parse(argc, argv)) {
    parser.usage(stderr, argc, argv);
    return 1;
  }

  if (show_version.value()) {
    fprintf(stdout, "SourcePawn version: %s\n", SOURCEPAWN_VERSION);
    fprintf(stdout, "API version: %x/%x\n", SOURCEPAWN_API_VERSION, SOURCEPAWN_ENGINE2_API_VERSION);
#if defined(SP_HAS_JIT)
    fprintf(stdout, "Just-in-time (JIT) compiler available.\n");
#endif
    return 0;
  }

  if ((sEnv = Environment::New()) == nullptr) {
    fprintf(stderr, "Could not initialize ISourcePawnEngine2\n");
    return 1;
  }

  if (getenv("DISABLE_JIT") || disable_jit.value())
    sEnv->SetJitEnabled(false);

  ShellDebugListener debug;
  sEnv->SetDebugger(&debug);

  if (!getenv("DISABLE_WATCHDOG") && !disable_watchdog.value())
    sEnv->InstallWatchdogTimer(5000);

  int errcode = Execute(filename.value().c_str());

  sEnv->SetDebugger(NULL);
  sEnv->Shutdown();
  delete sEnv;

  return errcode;
}
