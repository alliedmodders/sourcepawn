// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2012-2014 David Anderson
//
// This file is part of SourcePawn.
//
// SourcePawn is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option)
// any later version.
// 
// SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
#include "compile-context.h"
#include "compile-phases.h"
#include "source-manager.h"
#include "parser/preprocessor.h"
#include "parser/parser.h"
#include "sema/name-resolver.h"
#include "sema/semantic-analysis.h"
#include "shared/byte-buffer.h"
#include "smx/smx-compiler.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <amtl/os/am-fsutil.h>

using namespace ke;
using namespace sp;

ThreadLocal<CompileContext*> sp::CurrentCompileContext;

CompileContext::CompileContext(PoolAllocator& pool,
                               StringPool& strings,
                               ReportManager& reports,
                               SourceManager& source)
 : pool_(pool),
   strings_(strings),
   reports_(reports),
   source_(source),
   types_(strings)
{
  assert(!CurrentCompileContext);

  CurrentCompileContext = this;

  // We automatically add "include" from the current working directory.
  options_.SearchPaths.push_back(std::string("include/"));

  types_.initialize();
}

CompileContext::~CompileContext()
{
  CurrentCompileContext = NULL;
}

bool
CompileContext::ChangePragmaDynamic(ReportingContext& rc, int64_t value)
{
  if (value < 0) {
    rc.report(rmsg::pragma_dynamic_negative);
    return false;
  }
  if (uint64_t(value) >= 64 * kMB) {
    rc.report(rmsg::pragma_dynamic_too_large);
    return false;
  }

  options_.PragmaDynamic = size_t(value);
  return true;
}

static void
ReportMemory(FILE* fp)
{
  size_t allocated, reserved, bookkeeping;
  POOL().memoryUsage(&allocated, &reserved, &bookkeeping);

  fprintf(fp, " -- %" KE_FMT_SIZET " bytes allocated in pool\n", allocated);
  fprintf(fp, " -- %" KE_FMT_SIZET " bytes reserved in pool\n", reserved);
  fprintf(fp, " -- %" KE_FMT_SIZET " bytes used for bookkeeping\n", bookkeeping);
}

class FpBuffer : public ISmxBuffer
{
 public:
  explicit FpBuffer(FILE* fp)
   : fp_(fp)
  {}

  bool write(const void* bytes, size_t len) override {
    return fwrite(bytes, 1, len, fp_) == len;
  }
  size_t pos() const override {
    return (size_t)ftell(fp_);
  }

 private:
  FILE* fp_;
};

template <typename T> static inline T
LastMatch(T ptr, const char* search)
{
  T ext = strstr(ptr, search);
  while (ext) {
    T next_ext = strstr(ext + strlen(search), search);
    if (!next_ext)
      break;
    ext = next_ext;
  }
  return ext;
}

static std::string
GetOutputFilename(const char* output_file, const char* input_file)
{
  // :TODO: add a Strdup to amtl.
  size_t buffer_len = strlen(input_file) + 1;
  std::unique_ptr<char[]> buffer = std::make_unique<char[]>(buffer_len);
  SafeStrcpy(buffer.get(), buffer_len, input_file);

  if (!output_file) {
    // blah.sp -> blah.smx
    // .sp -> .sp.smx
    char* ext = LastMatch(buffer.get(), ".sp");
    if (ext && ext != buffer.get())
      *ext = '\0';

    std::string new_file = ke::StringPrintf("%s.smx", buffer.get());
    return new_file;
  }

  // /folder/crab, /blah/tmp.sp -> /folder/crab/tmp.smx
  if (ke::file::IsDirectory(output_file)) {
    std::string path(input_file);
    path = Join(Split(path, "\\"), "/");

    std::vector<std::string> parts = Split(path, "/");
    while (!parts.empty() && parts.back().length() == 0)
      parts.pop_back();
    if (!parts.empty()) {
      std::string smx_name = GetOutputFilename(nullptr, parts.back().c_str());
      std::string new_file = StringPrintf("%s/%s", output_file, smx_name.c_str());
      return new_file;
    }

    assert(false);
    return GetOutputFilename(nullptr, input_file);
  }

  // crab.smx -> crab.smx
  // crab -> crab.smx
  const char* ext = LastMatch(output_file, ".smx");
  if (ext && ext[4] == '\0')
    return std::string(output_file);

  return StringPrintf("%s.smx", output_file);
}

bool
CompileContext::compile(RefPtr<SourceFile> file)
{
  Preprocessor pp(*this);

  fprintf(stderr, "-- Parsing --\n");

  TranslationUnit* unit = new (pool()) TranslationUnit();
  {
    if (!pp.enter(file))
      return false;

    NameResolver nr(*this);
    Parser p(*this, pp, nr);
    ast::ParseTree* tree = p.parse();
    if (!phasePassed())
      return false;

    unit->attach(tree);
  }

  if (options_.ShowPoolStats) {
    ReportMemory(stderr);
    fprintf(stderr, "\n");
  }

  if (options_.ShowAST)
    unit->tree()->dump(stderr);

  if (options_.SkipSemanticAnalysis)
    return true;

  fprintf(stderr, "\n-- Semantic Analysis --\n");

  SemanticAnalysis sema(*this, unit);
  sema::Program* program = sema.analyze();
  if (!program)
    return false;

  if (options_.ShowPoolStats) {
    ReportMemory(stderr);
    fprintf(stderr, "\n");
  }

  if (options_.ShowSema)
    program->dump(stderr);

  std::string output_path = GetOutputFilename(
    options_.OutputFile ? (*options_.OutputFile).c_str() : nullptr,
    file->path());

  // Code generation.
  {
    SmxCompiler compiler(*this, program);
    if (!compiler.compile())
      return false;
    {
      FILE* fp = fopen(output_path.c_str(), "wt");
      FpBuffer buf(fp);
      if (!compiler.emit(&buf))
        return false;
      fclose(fp);
    }
  }

  if (reports_.HasErrors())
    return false;

  fprintf(stderr, "\n-- Ok! %s --\n", output_path.c_str());
  return true;
}

Atom*
CompileContext::createAnonymousName(const SourceLocation& loc)
{
  // :SRCLOC: include file name
  AutoString builder = "anonymous at ";
  builder = builder + source_.getLine(loc);
  builder = builder + source_.getCol(loc);
  return add(builder.ptr());
}
