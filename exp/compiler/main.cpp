/* vim: set ts=2 sw=2 tw=99 et:
 *
 * Copyright (C) 2012 David Anderson
 *
 * This file is part of SourcePawn.
 *
 * SourcePawn is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 * 
 * SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * SourcePawn. If not, see http://www.gnu.org/licenses/.
 */
#include <stdio.h>
#include <string.h>

#include <utility>

#include <amtl/experimental/am-argparser.h>
#include "compile-context.h"
#include "source-manager.h"
#include "type-manager.h"

using namespace ke;
using namespace ke::args;
using namespace sp;

int main(int argc, char** argv)
{
  Parser parser("SourcePawn compiler.");

  StringOption input_file(parser, "file", "Input file.");
  StringOption output_file(parser, "o", "output", Nothing(),
    "SMX output file.");
  RepeatOption<std::string> includes(parser, "-i", nullptr,
    "Add a folder to the include path.");

  // :TODO: Turn these off by default once we're closer to release.
  EnableOption show_ast(parser, nullptr, "show-ast", true,
    "Print the AST to stderr.");
  EnableOption show_sema(parser, nullptr, "show-sema", true,
    "Print the semantic analysis tree to stderr.");
  EnableOption pool_stats(parser, nullptr, "pool-stats", true,
    "Show pool memory usage after each phase.");
  ToggleOption parse_only(parser, nullptr, "parse-only", Some(false),
    "Skip name binding and type resolution.");
  ToggleOption bind_only(parser, nullptr, "bind-only", Some(false),
    "Skip type-checking and code generation.");

  if (!parser.parse(argc, argv)) {
    parser.usage(stderr, argc, argv);
    return 1;
  }

  StringPool strings;
  ReportManager reports;
  SourceManager source(strings, reports);

  PoolAllocator pool;
  {
    PoolScope scope(pool);
    CompileContext cc(pool, strings, reports, source);

    cc.options().SkipResolution = parse_only.value();
    cc.options().SkipSemanticAnalysis = bind_only.value();
    cc.options().ShowSema = show_sema.value();
    cc.options().ShowPoolStats = pool_stats.value();
    cc.options().OutputFile = output_file.maybeValue();
    cc.options().SearchPaths = std::move(includes.values());
    
    ReportingContext rc(cc, SourceLocation(), false);

    const char* filename = input_file.value().c_str();
    RefPtr<SourceFile> file = source.open(rc, filename);
    if (!file) {
      fprintf(stderr, "cannot open file '%s'\n", filename);
      return 1;
    }

    if (!cc.compile(file) || reports.HasMessages()) {
      reports.PrintMessages();
      return 1;
    }
  }

  return 0;
}

