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
#ifndef _include_spcomp_lexer_h_
#define _include_spcomp_lexer_h_

#include <amtl/am-refcounting.h>
#include "tokens.h"
#include "source-manager.h"
#include "process-options.h"
#include "pool-allocator.h"
#include "reporting.h"

namespace sp {

class CompileContext;
class Preprocessor;

typedef FixedPoolList<Token> TokenList;

class Lexer : public ke::Refcounted<Lexer>
{
 public:
  Lexer(CompileContext& cc, Preprocessor& pp, const LexOptions& options,
        RefPtr<SourceFile> buffer, const LREntry& range);

  const RefPtr<SourceFile>& buffer() const {
    return buffer_;
  }
  const LexOptions& options() const {
    return options_;
  }
  unsigned current_line() const {
    return line_number_;
  }

  bool processingDirective() const {
    return lexing_for_directive_;
  }

  // Lex one token.
  TokenKind next(Token* token);

  bool more() const {
    return canRead();
  }

 private:
  void checkIfStackAtEndOfFile();

 private:
  const char* ptr() const {
    return pos_;
  }
  char peekChar() {
    if (!canRead())
      return '\0';
    return *pos_;
  }
  void skipChar() {
    if (canRead())
      pos_++;
  }
  char readChar() {
    if (!canRead()) {
      scanned_eof_ = true;
      return '\0';
    }
    return *pos_++;
  }
  bool peekChar(char c) {
    return peekChar() == c;
  }
  bool matchChar(char c) {
    char got = readChar();
    if (got != c) {
      putBack(got);
      return false;
    }
    return true;
  }
  void putBack(char c) {
    // If the last character we consumed was an EOF, don't backtrack in the
    // stream. If we did scan eof_, all of the following should be true:
    //    pos_ == end_
    //    scanned_eof_
    //    c == '\0'
    // If we did not scan EOF, then pos_ can be == end_, but scanned_eof_
    // must be false.
    if (pos_ == end_ && scanned_eof_) {
      assert(c == '\0');
      return;
    }

    pos_--;

    assert(*pos_ == c);
    assert(!scanned_eof_);
    assert(canRead());
  }

  SourceLocation pos() const {
    return range_.filePos(pos_ - chars_);
  }
  SourceLocation lastpos() const {
    if (pos_ == chars_)
      return pos();
    return range_.filePos((pos_ - 1) - chars_);
  }

 private:
  // State for tracking #ifdef and such.
  struct IfContext
  {
    enum State {
      Active,   // Inside an #if/#else that evaluated to true.
      Ignoring, // Inside an #if that evaluated to false.
      Inactive, // Inside an #else after having been Active.
      Dead      // Pushed while not Active.
    };

    SourceLocation first;
    SourceLocation elseloc;
    State state;

    IfContext(const SourceLocation& first, State state)
     : first(first),
       state(state)
    {}
  };

  IfContext* currentIf() {
    return ifstack_.empty() ? nullptr : &ifstack_.back();
  }

 private:
  // The next readChar() will return the first non-space char.
  const char* skipSpaces();

  // Same as skipSpaces(), but performs a final read().
  char firstNonSpaceChar();

  // Read until the end of the line and trim any whitespace from the edges.
  void readUntilEnd(const char** beginp, const char** endp);

  // Lex the remainder of a hex literal.
  TokenKind hexLiteral();

  // Lex the remainder of a name or identifier.
  TokenKind name(char first);
  TokenKind maybeKeyword(char first);

  // Read an escape code for a string or character literal. Returns INT_MAX
  // on failure.
  int readEscapeCode();

  // Lex the remainder of a character literal.
  TokenKind charLiteral(Token* tok);

  // Lex the remainder of a string literal.
  TokenKind stringLiteral(Token* tok);

  // Lex the remainder of a number.
  TokenKind numberLiteral(char first);
  TokenKind handleNumber(Token* tok, char first);

  // Lex the remainder of a name, identifier, label, or macro.
  TokenKind handleIdentifier(Token* tok, char first);

  // Comment handling.
  TokenKind singleLineComment();
  TokenKind multiLineComment(const SourceLocation& begin);
  TokenKind processFrontCommentBlock(Token* tok);
  TokenKind processTailCommentBlock(Token* tok);
  TokenKind handleComments(Token* tok);

  // Consume whitespace and newlines, and then process the first token found.
  // This sets tok->start. If TOK_COMMENT is returned, tok->end is set as well.
  TokenKind scan(Token* tok);
  
  // This is the same as next(), except it can only be used inside a
  // preprocessor directive.
  TokenKind directive_next(Token* tok);

  void advanceLine(char c);
  char consumeWhitespace();

  TokenList* getMacroTokens();
  void chewLineAfterDirective(bool warn);
  void handleDirectiveWhileInactive();
  void handleIfContext();
  bool handlePreprocessorDirective();
  void enterPreprocessorDirective();

  MessageBuilder report(const SourceLocation& loc, rmsg::Id id);

 private:
  // Internal helpers.
  bool canRead() const {
    return pos_ < end_;
  }
  const char* literal() const {
    return literal_.data();
  }
  size_t literal_length() const {
    return literal_.size() - 1;
  }

 private:
  CompileContext& cc_;
  Preprocessor& pp_;
  LexOptions options_;
  RefPtr<SourceFile> buffer_;
  LREntry range_;
  const char* chars_;
  const char* pos_;
  const char* end_;
  std::vector<char> literal_;
  std::vector<IfContext> ifstack_;

  // Current line number.
  unsigned line_number_;

  // Whether or not we are lexing tokens for a preprocessor directive.
  bool lexing_for_directive_;

  // Set if we don't want to report errors.
  bool suppress_errors_;

  // If true, we have already lexed at least one token on this line.
  bool lexed_tokens_on_line_;

  // If true, the last character we scanned was EOF.
  bool scanned_eof_;
};

int StringToInt32(const char* ptr);

}

#endif // _include_spcomp_lexer_h_
