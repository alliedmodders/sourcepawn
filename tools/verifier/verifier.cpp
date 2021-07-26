// vim: set sts=2 ts=8 sw=2 tw=99 et:
// 
// Copyright (C) 2006-2016 AlliedModders LLC
// 
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#include "environment.h"
#include "method-verifier.h"
#include <set>
#include <deque>

using namespace ke;
using namespace sp;
using namespace SourcePawn;

Environment *sEnv = nullptr;
bool sVerbose = false;

static bool
Verify(IPluginRuntime* rt)
{
  ExceptionHandler eh(sEnv->APIv2());
  if (!rt->PerformFullValidation()) {
      const char* message = eh.HasException() ? eh.Message() : "unknown error";
      fprintf(stderr, "Binary validation failed: %s\n", message);
      return false;
  }
  return true;
}

static bool
Analyze(const char* file)
{
  char error[255];
  std::unique_ptr<IPluginRuntime> rt(sEnv->APIv2()->LoadBinaryFromFile(file, error, sizeof(error)));
  if (!rt) {
    fprintf(stdout, "Could not load .smx file: %s\n", error);
    return false;
  }

  if (sVerbose) {
    uint32_t index;
    if (rt->FindPubvarByName("myinfo", &index) == SP_ERROR_NONE) {
      cell_t local_addr;
      cell_t* info;

      IPluginContext* cx = rt->GetDefaultContext();
      cx->GetPubvarAddrs(index, &local_addr, &info);

      fprintf(stderr, "Plugin info:\n");
      for (size_t i = 0; i < 5; i++) {
        char* str;
        if (cx->LocalToString(info[i], &str) == SP_ERROR_NONE)
          fprintf(stderr, "%s\n", str);
      }
    }
  }

  return Verify(rt.get());
}

int main(int argc, char **argv)
{
  if (argc != 2) {
    fprintf(stderr, "Usage: <file>\n");
    return 1;
  }

  sVerbose = (getenv("VERBOSE") && getenv("VERBOSE")[0] == '1');

  if ((sEnv = Environment::New()) == nullptr) {
    fprintf(stderr, "Could not initialize ISourcePawnEngine2\n");
    return 1;
  }

  bool ok = Analyze(argv[1]);

  sEnv->Shutdown();
  delete sEnv;

  return ok ? 0 : 1;
}
