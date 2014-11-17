// vim: set ts=2 sw=2 tw=99 et:
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
#ifndef _include_spcomp_source_cache_h_
#define _include_spcomp_source_cache_h_

#include <am-utility.h>
#include <am-string.h>
#include "tokens.h"

namespace ke {

class SourceManager
{
 public:
  // Returns whether two source locations ultimately originate from the same
  // file (i.e., ignoring macros).
  bool sameFiles(const SourceLocation &a, const SourceLocation &b) {
    return false;
  }

  FileContext *getFile(const SourceLocation &loc) {
    return nullptr;
  }
  unsigned getLine(const SourceLocation &loc) {
    return 1;
  }
  unsigned getCol(const SourceLocation &loc) {
    return 0;
  }
};

}

#endif // _include_spcomp_source_cache_h_
