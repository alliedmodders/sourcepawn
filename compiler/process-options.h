// vim: set ts=2 sw=2 tw=99 et:
// 
// Copyright (C) 2012 David Anderson
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
#ifndef _include_spcomp_process_options_
#define _include_spcomp_process_options_

#include <am-vector.h>
#include <am-string.h>

namespace sp {

struct LexOptions
{
  bool RequireNewdecls;

  LexOptions()
   : RequireNewdecls(false)
  {}
};

struct CompileOptions
{
  // Always require new-style declarations.
  bool RequireNewdecls;

  // Always require semicolons.
  bool RequireSemicolons;

  // Skip name binding and type resolution.
  bool SkipResolution;

  // Memory size for v1 pcode.
  uint32_t PragmaDynamic;

  // Search paths.
  Vector<AString> SearchPaths;

  CompileOptions()
   : RequireNewdecls(false),
     RequireSemicolons(false),
     SkipResolution(false),
     PragmaDynamic(0)
  {
  }
};

} // namespace ke

#endif // _include_spcomp_process_options_
