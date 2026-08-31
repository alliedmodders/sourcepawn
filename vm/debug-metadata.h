// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2021-2026 AlliedModders LLC
//
#ifndef _include_sourcepawn_vm_debug_metadata_h_
#define _include_sourcepawn_vm_debug_metadata_h_

#include <vector>

#include <amtl/am-platform.h>
#include <stdint.h>

#if defined(KE_LINUX)
#    include <stdio.h>
#endif

namespace sp {

struct CodeDebugMapping {
    uint64_t addr;
    const char* file;
    uint32_t line;
};

using CodeDebugMap = std::vector<CodeDebugMapping>;

#if defined(KE_LINUX)
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
    void* mmap_;
    FILE* file_;

    // When being used with Intel PT profiling, we need to use the CPU clock as our time source.
    bool use_arch_timestamp_;
};
#endif

} // namespace sp

#endif // _include_sourcepawn_vm_debug_metadata_h_
