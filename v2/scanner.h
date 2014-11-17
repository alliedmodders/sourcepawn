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
#ifndef _include_jitcraft_scanner_h_
#define _include_jitcraft_scanner_h_

#include "tokens.h"
#include "compile-context.h"
#include "text-processor.h"
#include <am-vector.h>

namespace ke {

// This scanner assumes that the source has been preprocessed via
// Preprocessor.cpp. Any comments or directives not specifically
// emitted via the preprocessor, will produce an error.
class Scanner : public BasicLexer
{
  template <bool Enabled> friend class AutoAllowTags;

 public:
  Scanner(CompileContext &cc, TranslationUnit *tu);

  static double ParseDouble(const char *string);

  TokenKind next();
  TokenKind peek();

  Token *nextToken() {
    next();
    return current();
  }

  TokenKind peekTokenSameLine();

  // Put the last token back into the token stream.
  void undo();

  // Force an older token back into the stream.
  void pushBack(const Token &tok);

  // Beginning source position of the last instruction.
  SourceLocation begin() {
    return current()->start.loc;
  }
  SourceLocation end() {
    return current()->end.loc;
  }

  Token *current() {
    assert(num_tokens_ > 0);
    return &tokens_[cursor_];
  }
  Atom *current_name() {
    return current()->atom();
  }

  void eatRestOfLine();

  // These should only be used for name and keyword tokens.
  const char *literal() const {
    return BasicLexer::literal();
  }
  size_t literal_length() const {
    return BasicLexer::literal_length();
  }

  bool requireSemicolons() const {
    return require_semicolons_;
  }
  bool requireNewdecls() const {
    return require_newdecls_;
  }

 private:
  static const size_t kMaxLookahead = 4;

 private:
  int readEscapeCode();
  bool charLiteral(Token *tok);
  bool stringLiteral(Token *tok);
  void numberLiteral(Token *tok, char c);
  void identifier(Token *tok, char first);
  TokenKind scan();
  void readDirective();
  bool eatWhitespace();

  Token *cursorNext();
  Token *cursorPeek();
  void cursorPop();
  void cursorPrev();

  // Hide these so the parser can't accidentally use them wrong.
  Atom *name() const;

 private:
  TranslationUnit *tu_;

  size_t cursor_;
  size_t depth_;
  size_t num_tokens_;
  Token tokens_[kMaxLookahead];

  bool require_semicolons_;
  bool require_newdecls_;

  // :TODO: do something with this.
  bool pending_deprecate_;
  AString deprecation_message_;
};

} // namespace ke

#endif // _include_jitcraft_scanner_h_
