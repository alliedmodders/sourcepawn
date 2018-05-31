// vim: set sts=2 ts=8 sw=2 tw=99 et:
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
#include "keyword-table.h"
#include "compile-context.h"

using namespace ke;
using namespace sp;

const char* sp::TokenNames[] =
{
#define _(name, str) str,
  TOKENMAP(_)
#undef _
  nullptr
};

KeywordTable::KeywordTable(CompileContext& cc)
{
  impl_.init(128);

#define _(name)                                   \
  do {                                            \
    Atom* atom = cc.add(TokenNames[TOK_##name]);  \
    defineKeyword(atom, TOK_##name);              \
  } while (0);
  KEYWORDMAP(_)
#undef _
}

void
KeywordTable::defineKeyword(Atom* atom, TokenKind kind)
{
  Impl::Insert p = impl_.findForAdd(atom);
  assert(!p.found());

  impl_.add(p, atom, kind);
}
