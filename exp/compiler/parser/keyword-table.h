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
#ifndef _include_spcomp_keyword_table_h_
#define _include_spcomp_keyword_table_h_

#include "shared/string-pool.h"
#include "tokens.h"

namespace sp {

class CompileContext;

class KeywordTable
{
 public:
  KeywordTable(CompileContext& cc);

  TokenKind findKeyword(Atom* id) {
    Impl::Result r = impl_.find(id);
    if (!r.found())
      return TOK_NONE;
    return r->value;
  }

 private:
  void defineKeyword(Atom* atom, TokenKind tok);

 private:
  typedef AtomMap<TokenKind> Impl;

 private:
  Impl impl_;
};

}

#endif // _include_spcomp_keyword_table_h_
