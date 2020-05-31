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
#ifndef _include_spcomp_macros_h_
#define _include_spcomp_macros_h_

#include "shared/string-pool.h"
#include "pool-allocator.h"
#include "source-location.h"
#include "parser/tokens.h"
#include <stddef.h>

namespace sp {

typedef FixedPoolList<Token> TokenList;

struct Macro : public PoolObject
{
  // The location at which this macro was defined.
  SourceLocation definedAt;

  // The name of this macro.
  Atom* name;

  // The token buffer this macro expands to.
  TokenList* tokens;

  // True if this macro is currently being expanded.
  bool active;

  Macro(const SourceLocation& loc, Atom* name, TokenList* tokens)
   : definedAt(loc),
     name(name),
     tokens(tokens),
     active(false)
  {}

  // Return the location of the first token from the parent file.
  const SourceLocation& start() const {
    if (!tokens->size())
      return definedAt;
    return tokens->at(0).start.loc;
  }
  size_t length() const {
    if (tokens->size() == 0)
      return 0;
    return tokens->back().end.loc.offset() - tokens->at(0).start.loc.offset();
  }
};

} // namespace ke

#endif // _include_spcomp_macros_h_
