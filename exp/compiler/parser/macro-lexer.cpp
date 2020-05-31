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
#include "macro-lexer.h"
#include "preprocessor.h"

using namespace ke;
using namespace sp;

MacroLexer::MacroLexer(CompileContext& cc, Preprocessor& pp,
                       Macro* macro, const LREntry& range)
 : cc_(cc),
   pp_(pp)
{
  Reuse(macro, range);
}

void
MacroLexer::Reuse(Macro* macro, const LREntry& range)
{
  macro_ = macro;
  macro_->active = true;
  cursor_ = 0;
  range_ = range;
}

TokenKind
MacroLexer::next(Token* tok)
{
  if (cursor_ >= macro_->tokens->size()) {
    macro_->active = false;
    pp_.handleEndOfFile();
    return TOK_NONE;
  }

  const Token& first = macro_->tokens->at(0);
  const Token& current = macro_->tokens->at(cursor_);

  // Sanity checks.
  assert(cc_.source().sameSourceId(first.start.loc, current.start.loc));
  assert(cc_.source().sameSourceId(first.start.loc, current.end.loc));

  // Copy the next token, but rewrite the locations to be within the tracked
  // range. The line number for text in a macro is always 1. We compute the
  // offsets by taking the distance from the first token's start.
  *tok = macro_->tokens->at(cursor_);

  tok->init(TokenPos(range_.macroPos(current.start.loc.offset() - first.start.loc.offset()), 1),
            range_.id);
  tok->end.loc = range_.macroPos(current.end.loc.offset() - first.start.loc.offset());
  tok->end.line = 1;

  cursor_++;

  if (tok->kind == TOK_NAME && pp_.enterMacro(tok->start.loc, tok->atom())) {
    return (tok->kind = TOK_NONE);
  }
  return tok->kind;
}
