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
#include "compile-context.h"
#include "source-manager.h"
#include "type-manager.h"

using namespace ke;
using namespace sp;

int main(int argc, char **argv)
{
  if (argc != 2) {
    fprintf(stderr, "Usage: <file>\n");
    return 1;
  }

  StringPool strings;
  ReportManager reports;
  SourceManager source(strings, reports);

  PoolAllocator pool;
  {
    PoolScope scope(pool);
    CompileContext cc(pool, strings, reports, source);
    
    ReportingContext rc(cc, SourceLocation(), false);
    RefPtr<SourceFile> file = source.open(rc, argv[1]);
    if (!file) {
      fprintf(stderr, "cannot open file '%s'\n", argv[1]);
      return 1;
    }

    if (!cc.compile(file) || reports.HasMessages()) {
      reports.PrintMessages();
      return 1;
    }
  }

  return 0;
}

