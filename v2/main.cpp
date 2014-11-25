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

using namespace ke;

int main(int argc, char **argv)
{
  CompileContext cc(argc, argv);
  if (!cc.compile()) {
    for (size_t i = 0; i < cc.nerrors(); i++) {
      const CompileError &e = cc.getError(i);
      Ref<SourceFile> file = cc.source().getSource(e.loc);
      unsigned line = cc.source().getLine(e.loc);
      unsigned col = cc.source().getCol(e.loc);
      fprintf(stderr, "%s error (line %d, col %d): %s\n",
              file->path(),
              line,
              col,
              e.message);
    }
    return 1;
  }
}

