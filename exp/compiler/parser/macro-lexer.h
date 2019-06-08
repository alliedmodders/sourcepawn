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
#ifndef _include_spcomp_macro_lexer_h_
#define _include_spcomp_macro_lexer_h_

#include <amtl/am-refcounting.h>
#include "shared/string-pool.h"
#include "source-location.h"
#include "tokens.h"
#include "macros.h"
#include "source-manager.h"

namespace sp {

class CompileContext;
class Preprocessor;

// Macro lexers simply return tokens off a pre-lexed token list, rather than
// lexing a source stream.
class MacroLexer : public ke::Refcounted<MacroLexer>
{
 public:
  MacroLexer(CompileContext& cc, Preprocessor& pp,
             Macro* macro, const LREntry& range);

  void Reuse(Macro* macro, const LREntry& range);

  TokenKind next(Token* tok);

 private:
  CompileContext& cc_;
  Preprocessor& pp_;
  Macro* macro_;
  SourceLocation insertion_loc_;
  size_t cursor_;
  LREntry range_;
};

} // namespace ke

#endif // _include_spcomp_macro_lexer_h_
