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
  bool TraceComments;

  LexOptions()
   : RequireNewdecls(false),
     TraceComments(false)
  {}
};

struct CompileOptions
{
  bool RequireNewdecls;
  bool RequireSemicolons;
  bool DumpAST;
  size_t PragmaDynamic;
  Vector<AString> SearchPaths;
  Vector<AString> InputFiles;

  CompileOptions()
   : RequireNewdecls(false),
     RequireSemicolons(false),
     DumpAST(false),
     PragmaDynamic(0)
  {
  }
};

} // namespace ke

#endif // _include_spcomp_process_options_
