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
#include "debug-metadata.h"

#include <amtl/am-string.h>

using namespace sp;

#if defined(KE_LINUX) && defined(SP_HAS_JIT)
#include "jitdump.h"

#include <unistd.h>

// clock_gettime
#include <time.h>

// mmap
#include <sys/mman.h>

// creat
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

// __rdtsc
#include <x86intrin.h>

PerfJitFile::PerfJitFile(bool self_delete)
{
  self_delete_ = self_delete;

  // The specification requires the file to be located at exactly this path.
  // https://github.com/torvalds/linux/blob/614cb5894306cfa2c7d9b6168182876ff5948735/tools/perf/Documentation/jit-interface.txt
  ke::SafeSprintf(path_, sizeof(path_), "/tmp/perf-%d.map", getpid());

  file_ = fopen(path_, "w");
}

PerfJitFile::~PerfJitFile()
{
  if (!file_) {
    return;
  }

  fclose(file_);

  if (self_delete_) {
    unlink(path_);
  }
}

void PerfJitFile::Write(void* address, uint64_t length, const char* symbol) {
  if (!file_) {
    return;
  }

  fprintf(file_,
    "%" KE_FMT_X64 " %" KE_FMT_X64 " %s\n",
    (uint64_t)address, length, symbol);

  fflush(file_);
}

PerfJitdumpFile::PerfJitdumpFile(bool self_delete) {
  pid_ = getpid();
  self_delete_ = self_delete;

  // We can keep this file anywhere, but the name must be ".../jit-XXXX.dump", where XXXX is our PID.
  // https://github.com/torvalds/linux/blob/614cb5894306cfa2c7d9b6168182876ff5948735/tools/perf/Documentation/jitdump-specification.txt
  ke::SafeSprintf(path_, sizeof(path_), "/tmp/jit-%d.dump", pid_);

  // We have to explicitly open a fd or perf can't find the mmap, using fopen and fileno doesn't work.
  int fd = open(path_, O_CREAT|O_TRUNC|O_RDWR, 0666);
  if (fd < 0) {
    return;
  }

  // This mmap acts as a marker for perf to find our jitdump file path.
  mmap_ = mmap(nullptr, sysconf(_SC_PAGESIZE), PROT_READ|PROT_EXEC, MAP_PRIVATE, fd, 0);
  if (mmap_ == MAP_FAILED) {
    mmap_ = nullptr;
  }

  file_ = fdopen(fd, "w+");
  if (!file_) {
    close(fd);
    return;
  }

  // Use arch timestamps unless the env var is explicitly set to 0.
  char* env = getenv("JITDUMP_USE_ARCH_TIMESTAMP");
  use_arch_timestamp_ = !env || *env != '0';

  jitheader header = {};
  header.magic = JITHEADER_MAGIC;
  header.version = JITHEADER_VERSION;
  header.total_size = sizeof(header);
  header.elf_mach = GetElfMachine();
  header.pid = pid_;
  header.timestamp = GetTimestamp();
  header.flags = use_arch_timestamp_ ? JITDUMP_FLAGS_ARCH_TIMESTAMP : 0;

  fwrite(&header, sizeof(header), 1, file_);

  fflush(file_);
}

PerfJitdumpFile::~PerfJitdumpFile() {
  if (!file_) {
    return;
  }

  // Write close record?

  fclose(file_);

  if (self_delete_) {
    unlink(path_);
  }
}

void PerfJitdumpFile::Write(void* address, uint64_t length, const char* symbol, const CodeDebugMap& mapping) {
  if (!file_) {
    return;
  }

  if (!mapping.empty()) {
    jr_code_debug_info dbg_record = {};
    dbg_record.p.id = JIT_CODE_DEBUG_INFO;
    dbg_record.p.total_size = sizeof(dbg_record);
    dbg_record.p.timestamp = GetTimestamp();
    dbg_record.code_addr = (uint64_t)address;
    dbg_record.nr_entry = mapping.size();

    for (const auto& map : mapping) {
      dbg_record.p.total_size += sizeof(debug_entry) + strlen(map.file) + 1;
    }

    fwrite(&dbg_record, sizeof(dbg_record), 1, file_);

    for (const auto& map : mapping) {
      // Our special markers can be identified by them having a line number of 0,
      // perf doesn't like that so we just shift them up to 1 so they're not lost.
      debug_entry entry = {};
      entry.addr = (uint64_t)address + map.addr;
      entry.lineno = map.line >= 1 ? map.line : 1;

      fwrite(&entry, sizeof(entry), 1, file_);
      fwrite(map.file, 1, strlen(map.file) + 1, file_);
    }
  }

  jr_code_load record = {};
  record.p.id = JIT_CODE_LOAD;
  record.p.total_size = sizeof(record) + strlen(symbol) + 1 + length;
  record.p.timestamp = GetTimestamp();
  record.pid = pid_;
  record.tid = pid_; // TODO: Use the thread id.
  record.vma = (uint64_t)address;
  record.code_addr = (uint64_t)address;
  record.code_size = length;
  record.code_index = (uint64_t)address; // We don't re-JIT, so this should be fine.

  fwrite(&record, sizeof(record), 1, file_);
  fwrite(symbol, 1, strlen(symbol) + 1, file_);
  fwrite(address, 1, length, file_);

  fflush(file_);
}

uint64_t PerfJitdumpFile::GetTimestamp() {
  timespec ts;

  if (use_arch_timestamp_) {
    return __rdtsc();
  }

  if (clock_gettime(CLOCK_MONOTONIC, &ts)) {
    return 0;
  }

  return ((uint64_t)ts.tv_sec * 1000000000) + ts.tv_nsec;
}

uint16_t PerfJitdumpFile::GetElfMachine() {
  uint16_t machine = 0; /* EM_NONE */

  FILE* exe = fopen("/proc/self/exe", "r");
  if (!exe) {
    return machine;
  }

  struct {
    uint8_t ident[16];
    uint16_t type;
    uint16_t machine;
  } __attribute__((packed)) elf_header;

  if (fread(&elf_header, sizeof(elf_header), 1, exe) != 0) {
    machine = elf_header.machine;
  }

  fclose(exe);
  return machine;
}
#endif
