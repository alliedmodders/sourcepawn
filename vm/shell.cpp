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
#include <am-cxx.h>
#include "dll_exports.h"
#include "environment.h"
#include "stack-frames.h"
#include <sp_native_api.h>

using namespace ke;
using namespace sp;
using namespace SourcePawn;

Environment *sEnv;

static void
DumpStack(IFrameIterator &iter)
{
  int index = 0;
  for (; !iter.Done(); iter.Next(), index++) {
    const char *name = iter.FunctionName();
    if (!name) {
      fprintf(stdout, "  [%d] <unknown>\n", index);
      continue;
    }

    if (iter.IsScriptedFrame()) {
      const char *file = iter.FilePath();
      if (!file)
        file = "<unknown>";
      fprintf(stdout, "  [%d] %s::%s, line %d\n", index, file, name, iter.LineNumber());
    } else {
      fprintf(stdout, "  [%d] %s()\n", index, name);
    }
  }
}

class ShellDebugListener : public IDebugListener
{
public:
  void ReportError(const IErrorReport &report, IFrameIterator &iter) KE_OVERRIDE {
    fprintf(stdout, "Exception thrown: %s\n", report.Message());
    DumpStack(iter);
  }

  void OnDebugSpew(const char *msg, ...) {
#if !defined(NDEBUG) && defined(DEBUG)
    va_list ap;
    va_start(ap, msg);
    vfprintf(stderr, msg, ap);
    va_end(ap);
#endif
  }
};

static int Print(const char* s)
{
  return printf("%s", s);
}

static int PrintNum(int num)
{
  return printf("%d\n", num);
}

static cell_t PrintNums(IPluginContext *cx, const CellArgs& args)
{
  for (size_t i = 0; i < args.count(); i++) {
    int err;
    cell_t *addr;
    if ((err = cx->LocalToPhysAddr(args[i], &addr)) != SP_ERROR_NONE)
      return cx->ThrowNativeErrorEx(err, "Could not read argument");
    fprintf(stdout, "%d", *addr);
    if (i != args.count())
      fprintf(stdout, ", ");
  }
  fprintf(stdout, "\n");
  return 1;
}

static void DoNothing()
{
}

static float PrintFloat(float f)
{
  return printf("%f\n", f);
}

static cell_t DoExecute(IPluginContext* cx, cell_t fn_id, cell_t count)
{
  int32_t ok = 0;
  for (size_t i = 0; i < size_t(count); i++) {
    if (IPluginFunction *fn = cx->GetFunctionById(fn_id)) {
      if (fn->Execute(nullptr) != SP_ERROR_NONE)
        continue;
      ok++;
    }
  }
  return ok;
}

static cell_t DoInvoke(IPluginContext* cx, cell_t fn_id, cell_t count)
{
  for (size_t i = 0; i < size_t(count); i++) {
    if (IPluginFunction *fn = cx->GetFunctionById(fn_id)) {
      if (!fn->Invoke())
        return 0;
    }
  }
  return 1;
}

static cell_t DumpStackTrace()
{
  FrameIterator iter;
  DumpStack(iter);
  return 0;
}

static int Execute(const char *file)
{
  char error[255];
  AutoPtr<IPluginRuntime> rt(sEnv->APIv2()->LoadBinaryFromFile(file, error, sizeof(error)));
  if (!rt) {
    fprintf(stderr, "Could not load plugin: %s\n", error);
    return 1;
  }

  IPluginFunction *fun = rt->GetFunctionByName("main");
  if (!fun)
    return 0;

  IPluginContext *cx = rt->GetDefaultContext();

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

int main(int argc, char **argv)
{
  if (argc != 2) {
    fprintf(stderr, "Usage: <file>\n");
    return 1;
  }

  if ((sEnv = Environment::New()) == nullptr) {
    fprintf(stderr, "Could not initialize ISourcePawnEngine2\n");
    return 1;
  }

  if (getenv("DISABLE_JIT"))
    sEnv->SetJitEnabled(false);

  ShellDebugListener debug;
  sEnv->SetDebugger(&debug);
  sEnv->InstallWatchdogTimer(5000);

  //sEnv->AddNative("print",      Deduce(Print));
  sEnv->AddNative("printnum",   Deduce(PrintNum));
  sEnv->AddNative("printnums",  Deduce(PrintNums));
  sEnv->AddNative("printfloat", Deduce(PrintFloat));
  sEnv->AddNative("donothing",  Deduce(DoNothing));
  sEnv->AddNative("execute",    Deduce(DoExecute));
  sEnv->AddNative("invoke",     Deduce(DoInvoke));
  sEnv->AddNative("dump_stack_trace", Deduce(DumpStackTrace));

  int errcode = Execute(argv[1]);

  sEnv->SetDebugger(NULL);
  sEnv->Shutdown();
  delete sEnv;

  return errcode;
}
