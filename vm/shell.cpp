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
#include <sstream>

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
ucell_t sDebugBreakCip = -1;
static const char* scope_names[] = {"glb", "loc", "sta", "arg"};

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

std::string
TypeToString(const ISymbolType* type) {
  std::stringstream output;
  if (type->isConstant())
    output << "const ";

  if (type->isReference())
    output << "&";

  if (type->isBoolean())
    output << "bool";
  else if (type->isInt32())
    output << "int";
  else if (type->isFloat32())
    output << "float";
  else if (type->isString())
    output << "char";
  else if (type->isAny())
    output << "any";
  else if (type->isFunction())
    output << "Function";
  else if (type->isVoid())
    output << "void";
  else if (type->isEnum() || type->isEnumStruct() || type->isStruct() || type->isObject())
    output << type->name();
  else
    output << "<unknown>";

  if (type->isArray()) {
    for (uint32_t dim = 0; dim < type->dimcount(); dim++) {
      if (type->dimension(dim) > 0)
        output << '[' << type->dimension(dim) << ']';
      else {
        output << "[]";
      }
    }
  }
  return output.str();
}

void
OnDebugBreak(IPluginContext* ctx, sp_debug_break_info_t& dbginfo,
             const IErrorReport* report) {
  if (dbginfo.cip != sDebugBreakCip)
    return;

  IPluginDebugInfo* debuginfo = ctx->GetRuntime()->GetDebugInfo();
  IDebugSymbolIterator* symbol_iterator = debuginfo->CreateSymbolIterator(dbginfo.cip);

  // Print visible variables in local scope.
  uint32_t index = 0;
  while (!symbol_iterator->Done()) {
    const SourcePawn::IDebugSymbol* sym = symbol_iterator->Next();
      if (sym->scope() == Global)
        continue;

    fprintf(stdout, "[dbg] Symbol #%d:\n", index);
    // fprintf(stdout, "[dbg]   address: %08x\n", sym->address());
    // fprintf(stdout, "[dbg]   codestart: %08x\n", sym->codestart());
    // fprintf(stdout, "[dbg]   codeend: %08x\n", sym->codeend());
    fprintf(stdout, "[dbg]   name: %s\n", sym->name() ? sym->name() : "<unknown>");
    fprintf(stdout, "[dbg]   scope: %s\n", scope_names[sym->scope()]);
    fprintf(stdout, "[dbg]   type: %s\n", TypeToString(sym->type()).c_str());

    ++index;
  }

  debuginfo->DestroySymbolIterator(symbol_iterator);
}

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

static int Execute(const char* file, uint32_t break_line)
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

  if (break_line > 0) {
    if (rt->NumFiles() == 0) {
      fprintf(stderr, "Couldn't find source file name for debug break.\n");
      return 1;
    }

    const char* filename = rt->GetFileName(rt->NumFiles() - 1);
    if (rt->LookupLineAddress(break_line - 1, filename, &sDebugBreakCip) != SP_ERROR_NONE) {
      fprintf(stderr, "Error finding debug break line %s:%d\n", filename, break_line);
      return 1;
    }
  }

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
  ToggleOption validate_debug_sections(parser,
    "d", "validate-debug-sections",
    Some(false),
    "Validate debug sections before loading the plugin. Enables line debugging in the runtime, which might slow down execution.");
  StringOption debug_break_line(parser,
    "dbg", "debug-break-line",
    Some(std::string()),
    "Line to break at and dump the available debug state.");
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

  uint32_t break_line = 0;
  if (getenv("DEBUG_BREAK_LINE") || !debug_break_line.value().empty()) {
    std::string break_line_str;
    if (getenv("DEBUG_BREAK_LINE"))
      break_line_str = getenv("DEBUG_BREAK_LINE");
    else
      break_line_str = debug_break_line.value();

    break_line = strtoul(break_line_str.c_str(), nullptr, 10);
    sEnv->SetDebugBreakHandler(OnDebugBreak);
  }

  if (break_line > 0 || getenv("VALIDATE_DEBUG_SECTIONS") || validate_debug_sections.value())
    sEnv->EnableDebugBreak();

  ShellDebugListener debug;
  sEnv->SetDebugger(&debug);

  if (!getenv("DISABLE_WATCHDOG") && !disable_watchdog.value())
    sEnv->InstallWatchdogTimer(5000);

  int errcode = Execute(filename.value().c_str(), break_line);

  sEnv->SetDebugger(NULL);
  sEnv->Shutdown();
  delete sEnv;

  return errcode;
}
