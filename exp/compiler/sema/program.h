// vim: set ts=2 sw=2 tw=99 et:
// 
// Copyright (C) 2012-2014 AlliedModders LLC and David Anderson
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
#ifndef _include_semantic_analysis_program_h_
#define _include_semantic_analysis_program_h_

#include <amtl/am-vector.h>
#include <stdio.h>
#include "pool-allocator.h"

namespace sp {

namespace ast {
class FunctionStatement;
class VarDecl;
} // namespace ast

namespace sema {

class FunctionDef;

struct Program : public PoolObject
{
  std::vector<ast::FunctionStatement*> functions;
  std::vector<ast::VarDecl*> globals;

  void dump(FILE* fp);
};

} // namespace sema
} // namespace sp

#endif // _include_semantic_analysis_program_h_
