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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

using namespace ke;
using namespace sp;

ThreadLocal<CompileContext *> sp::CurrentCompileContext;

CompileContext::CompileContext(PoolAllocator &pool,
                               StringPool &strings,
                               ReportManager &reports,
                               SourceManager &source)
 : pool_(pool),
   strings_(strings),
   reports_(reports),
   source_(source),
   types_(strings)
{
  assert(!CurrentCompileContext);

  CurrentCompileContext = this;

  // We automatically add "include" from the current working directory.
  options_.SearchPaths.append(AString("include/"));

  types_.initialize();
}

CompileContext::~CompileContext()
{
  CurrentCompileContext = NULL;
}

bool
CompileContext::ChangePragmaDynamic(ReportingContext &rc, int64_t value)
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
ReportMemory(FILE *fp)
{
  size_t allocated, reserved, bookkeeping;
  POOL().memoryUsage(&allocated, &reserved, &bookkeeping);

  fprintf(fp, " -- %" KE_FMT_SIZET " bytes allocated in pool\n", allocated);
  fprintf(fp, " -- %" KE_FMT_SIZET " bytes reserved in pool\n", reserved);
  fprintf(fp, " -- %" KE_FMT_SIZET " bytes used for bookkeeping\n", bookkeeping);
}

bool
CompileContext::compile(RefPtr<SourceFile> file)
{
  Preprocessor pp(*this);

  fprintf(stderr, "-- Parsing --\n");

  TranslationUnit *unit = new (pool()) TranslationUnit();
  {
    if (!pp.enter(file))
      return false;

    NameResolver nr(*this);
    Parser p(*this, pp, nr);
    ast::ParseTree *tree = p.parse();
    if (!phasePassed())
      return false;

    unit->attach(tree);
  }

  ReportMemory(stderr);
  fprintf(stderr, "\n");

  unit->tree()->dump(stderr);

  if (options_.SkipSemanticAnalysis)
    return true;

  fprintf(stderr, "\n-- Semantic Analysis --\n");

  SemanticAnalysis sema(*this, unit);
  sema::Program* program = sema.analyze();
  if (!program)
    return false;

  ReportMemory(stderr);
  fprintf(stderr, "\n");

  program->dump(stderr);

  {
    //AmxEmitter sema(*this, units_[0]);
    //if (!sema.compile())
    //  return false;
  }

  if (reports_.HasErrors())
    return false;

  return true;
}

Atom *
CompileContext::createAnonymousName(const SourceLocation &loc)
{
  // :SRCLOC: include file name
  AutoString builder = "anonymous at ";
  builder = builder + source_.getLine(loc);
  builder = builder + source_.getCol(loc);
  return add(builder.ptr());
}
