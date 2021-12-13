// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2021 AlliedModders LLC
//
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#ifndef _include_sourcepawn_vm_debug_metadata_h_
#define _include_sourcepawn_vm_debug_metadata_h_

#include <vector>

#include <stdint.h>
#include <amtl/am-platform.h>

#if defined(KE_LINUX)
#include <stdio.h>
#endif

namespace sp {

struct CodeDebugMapping {
  uint64_t addr;
  const char* file;
  uint32_t line;
};

using CodeDebugMap = std::vector<CodeDebugMapping>;

#if defined(KE_LINUX) && defined(SP_HAS_JIT)
class PerfJitFile
{
 public:
  explicit PerfJitFile(bool self_delete);
  ~PerfJitFile();

  void Write(void* address, uint64_t length, const char* symbol);

 private:
  bool self_delete_;
  char path_[255];
  FILE* file_;
};

class PerfJitdumpFile
{
 public:
  explicit PerfJitdumpFile(bool self_delete);
  ~PerfJitdumpFile();

  void Write(void* address, uint64_t length, const char* symbol, const CodeDebugMap& mapping);

 private:
  uint64_t GetTimestamp();
  uint16_t GetElfMachine();

 private:
  int pid_;
  bool self_delete_;
  char path_[255];
  void *mmap_;
  FILE* file_;

  // When being used with Intel PT profiling, we need to use the CPU clock as our time source.
  bool use_arch_timestamp_;
};
#endif

} // namespace sp

#endif // _include_sourcepawn_vm_debug_metadata_h_
