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
#include "lexer.h"
#include "compile-context.h"
#include "preprocessor.h"
#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <amtl/am-arithmetic.h>

using namespace ke;
using namespace sp;

Lexer::Lexer(CompileContext& cc, Preprocessor& pp, const LexOptions& options,
             RefPtr<SourceFile> buffer, const LREntry& range)
 : cc_(cc),
   pp_(pp),
   options_(options),
   buffer_(buffer),
   range_(range),
   chars_(buffer_->chars()),
   pos_(buffer_->chars()),
   end_(buffer_->chars() + buffer_->length()),
   line_number_(1),
   lexing_for_directive_(false),
   suppress_errors_(false),
   lexed_tokens_on_line_(false),
   scanned_eof_(false)
{
}

MessageBuilder
Lexer::report(const SourceLocation& loc, rmsg::Id id)
{
  if (suppress_errors_)
    return MessageBuilder(nullptr);

  return cc_.report(loc, id);
}

static inline bool IsDigit(char c)
{
  return c >= '0' && c <= '9';
}

static inline bool IsHexDigit(char c)
{
  return IsDigit(c) ||
       (c >= 'a' && c <= 'f') ||
       (c >= 'A' && c <= 'F');
}

static inline bool IsLineTerminator(char c)
{
  return c == '\n' || c == '\r' || c == '\0';
}

static inline bool IsSkipSpace(char c)
{
  return c == ' ' || c == '\t' || c == '\f';
}

static inline bool IsIdentStart(char c)
{
  return (c >= 'a' && c <= 'z') ||
         (c >= 'A' && c <= 'Z') ||
          c == '_';
}

static inline bool IsIdentChar(char c)
{
  return IsIdentStart(c) || (c >= '0' && c <= '9');
}

int
sp::StringToInt32(const char* ptr)
{
  int v = 0;
  while (IsDigit(*ptr) || *ptr == '_') {
    if (*ptr != '_')
      v = (v * 10) + (*ptr - '0');
    ptr++;
  }
  return v;
}


const char*
Lexer::skipSpaces()
{
  while (IsSkipSpace(peekChar()))
    skipChar();
  return ptr();
}

char
Lexer::firstNonSpaceChar()
{
  char c = readChar();
  while (IsSkipSpace(c))
    c = readChar();
  return c;
}

void
Lexer::readUntilEnd(const char** beginp, const char** endp)
{
  const char* begin = skipSpaces();

  while (!IsLineTerminator(peekChar()))
    skipChar();

  const char* end = ptr();
  while (end > begin) {
    if (!IsSkipSpace(*end) && !IsLineTerminator(*end))
      break;
    end--;
  }

  *beginp = begin;
  *endp = end + 1;
}

TokenKind
Lexer::hexLiteral()
{
  literal_.clear();
  for (;;) {
    char c = readChar();
    if (!IsHexDigit(c)) {
      putBack(c);
      break;
    }
    literal_.push_back(c);
  }
  literal_.push_back('\0');
  return TOK_HEX_LITERAL;
}

TokenKind
Lexer::numberLiteral(char first)
{
  literal_.clear();
  literal_.push_back(first);

  char c;
  for (;;) {
    c = readChar();
    if (!IsDigit(c))
      break;
    literal_.push_back(c);
  }

  // Detect a hexadecimal string.
  if (literal_.size() == 1 &&
      literal_[0] == '0' &&
      (c == 'x' || c == 'X'))
  {
    return hexLiteral();
  }

  if (c != '.') {
    putBack(c);
    literal_.push_back('\0');
    return TOK_INTEGER_LITERAL;
  }
  literal_.push_back(c);

  c = readChar();
  if (!IsDigit(c)) {
    char print[2] = {c, '\0'};
    cc_.report(pos(), rmsg::expected_digit_for_float)
      << print;
    return TOK_UNKNOWN;
  }
  literal_.push_back(c);

  for (;;) {
    c = readChar();
    if (!IsDigit(c)) {
      putBack(c);
      break;
    }
    literal_.push_back(c);
  }

  if (!matchChar('e')) {
    literal_.push_back('\0');
    return TOK_FLOAT_LITERAL;
  }

  literal_.push_back(c);
  c = readChar();
  if (c == '-') {
    literal_.push_back(c);
    c = readChar();
  }
  if (!IsDigit(c)) {
    putBack(c);

    char print[2] = {c, '\0'};
    cc_.report(pos(), rmsg::expected_digit_for_float)
      << print;
    return TOK_UNKNOWN;
  }
  literal_.push_back(c);
  for (;;) {
    c = readChar();
    if (!IsDigit(c)) {
      putBack(c);
      break;
    }
    literal_.push_back(c);
  }
  
  literal_.push_back('\0');
  return TOK_FLOAT_LITERAL;
}

static inline uint64_t
HexDigitToValue(char c)
{
  if (IsDigit(c))
    return c - '0';
  if (c >= 'a') {
    assert(c <= 'f');
    return (c - 'a') + 10;
  }
  assert(c >= 'A' && c <= 'F');
  return (c - 'A') + 10;
}

// Based off the logic in sc2.c's ftoi()...
static double
ParseDouble(const char* string)
{
  const char* ptr = string;

  double number = 0.0;
  while (IsDigit(*ptr)) {
    number = (number * 10) + (*ptr - '0');
    ptr++;
  }

  assert(*ptr == '.');
  ptr++;

  double fraction = 0.0;
  double multiplier = 1.0;
  while (IsDigit(*ptr)) {
    fraction = (fraction * 10) + (*ptr - '0');
    multiplier = multiplier / 10.0;
    ptr++;
  }

  number += fraction * multiplier;

  if (*ptr++ == 'e') {
    int sign = 1;
    if (*ptr == '-') {
      sign = -1;
      ptr++;
    }

    int exponent = 0;
    while (IsDigit(*ptr)) {
      exponent = (exponent * 10) + (*ptr - '0');
      ptr++;
    }

    multiplier = pow(10.0, exponent * sign);
    number *= multiplier;
  }

  return number;
}

TokenKind
Lexer::handleNumber(Token* tok, char first)
{
  TokenKind kind = numberLiteral(first);
  switch (kind) {
    case TOK_INTEGER_LITERAL:
    {
      uint64_t val = 0;
      for (size_t i = 0; i < literal_length(); i++) {
        assert(IsDigit(literal_[i]));
        if (!TryUint64Multiply(val, 10, &val)) {
          report(tok->start.loc, rmsg::int_literal_overflow);
          break;
        }
        if (!TryUint64Add(val, (literal_[i] - '0'), &val)) {
          report(tok->start.loc, rmsg::int_literal_overflow);
          break;
        }
      }
      tok->setIntValue(val);
      break;
    }

    case TOK_HEX_LITERAL:
    {
      uint64_t val = 0;
      for (size_t i = 0; i < literal_length(); i++) {
        uint64_t digit = HexDigitToValue(literal_[i]);
        if (!TryUint64Multiply(val, 16, &val)) {
          report(tok->start.loc, rmsg::int_literal_overflow);
          break;
        }
        if (!TryUint64Add(val, digit, &val)) {
          report(tok->start.loc, rmsg::int_literal_overflow);
          break;
        }
      }
      tok->setIntValue(val);
      break;
    }

    case TOK_FLOAT_LITERAL:
      tok->setDoubleValue(ParseDouble(literal()));
      break;

    default:
      assert(false);
  }

  return kind;
}

TokenKind
Lexer::name(char first)
{
  literal_.clear();
  literal_.push_back(first);
  char c;
  for (;;) {
    c = readChar();
    if (!IsIdentChar(c)) {
      putBack(c);
      break;
    }
    literal_.push_back(c);
  }
  literal_.push_back('\0');
  return TOK_NAME;
}

TokenKind
Lexer::maybeKeyword(char first)
{
  name(first);

  Atom* atom = cc_.add(literal(), literal_length());
  return pp_.findKeyword(atom);
}

// Based on the logic for litchar() in sc2.c.
int
Lexer::readEscapeCode()
{
  char c = readChar();
  if (c == '\\')
    return c;

  switch (c) {
    case 'a':
      return '\a';
    case 'b':
      return '\b';
    case 'e':
      return 27;      // Apparently \e is non-standard.
    case 'f':
      return '\f';
    case 'n':
      return '\n';
    case 'r':
      return '\r';
    case 't':
      return '\t';
    case 'v':
      return '\v';

    case 'x':
    {
      unsigned digits = 0;
      char r = 0;
  
      c = readChar();
      while (IsHexDigit(c) && digits < 2) {
        if (IsDigit(c))
          r = (r << 4) + (c - '0');
        else
          r = (r << 4) + (tolower(c) - 'a' + 10);
        digits++;
        c = readChar();
       }
        
      // Swallow a trailing ';'
      if (c != ';')
        putBack(c);
  
      return r;
    }

    case '\'':
    case '\"':
    case '%':
      return c;

    default:
    {
      if (IsDigit(c)) {
        // \ddd
        char r = 0;
        while (IsDigit(c)) {
          r = r * 10 + (c - '0');
          c = readChar();
        }

        // Swallow a trailing ;
        if (c != ';')
          putBack(c);

        return r;
      }
      
      char print[2] = {c, '\0'};
      cc_.report(lastpos(), rmsg::unknown_escapecode)
        << print;
      return INT_MAX;
    }
  }
}

TokenKind
Lexer::charLiteral(Token* tok)
{
  char c = readChar();
  if (c == '\'') {
    report(tok->start.loc, rmsg::invalid_char_literal);
    return TOK_UNKNOWN;
  }

  tok->kind = TOK_CHAR_LITERAL;
  if (c == '\\')
    tok->setCharValue(readEscapeCode());
  else
    tok->setCharValue(c);

  c = readChar();
  if (c != '\'') {
    report(tok->start.loc, rmsg::bad_char_terminator);

    // If the user did something like '5", assume it was a typo and keep the
    // token. Otherwise, backtrack.
    if (c != '"')
      putBack(c);
  }

  return tok->kind;
}

TokenKind
Lexer::stringLiteral(Token* tok)
{
  literal_.clear();

  for (;;) {
    char c = readChar();
    if (c == '\"')
      break;
    if (c == '\r' || c == '\n' || c == '\0') {
      cc_.report(tok->start.loc, rmsg::unterminated_string);
      break;
    }
    if (c == '\\') {
      // Peek ahead - if this is the last non-whitespace character on the line,
      // we have to act as if the line was concatenated.
      const char* ptr = pos_;
      while (*ptr && IsSkipSpace(*ptr))
        ptr++;
      if (*ptr == '\n' || *ptr == '\r') {
        // Just move to the next line. Note, advanceLine expects* pos_-1 == c.
        pos_ = ptr + 1;
        advanceLine(*ptr);
        continue;
      }

      int code = readEscapeCode();
      if (code == INT_MAX)
        code = '?';
      c = char(code);
    }
    literal_.push_back(c);
  }

  literal_.push_back('\0');

  tok->setAtom(cc_.add(literal(), literal_length()));
  return TOK_STRING_LITERAL;
}

TokenKind
Lexer::handleIdentifier(Token* tok, char first)
{
  name(first);

  Atom* atom = cc_.add(literal(), literal_length());
  tok->setAtom(atom);

  // Strictly speaking, it is not safe to handle macro expansion directly
  // as we lex the token. But it's hard to really tell when it's safe. The
  // problem is that lookahead could want the actual underlying NAME token,
  // but we've expanded it too early.
  //
  // For now, we're just careful. We don't lookahead into preprocessor
  // directives, and that should make it safe to disable expansion right before
  // we start lexing un-expanded TOK_NAMES.
  if (pp_.macro_expansion() && pp_.enterMacro(tok->start.loc, atom)) {
    // No matter what the macro expands to (even if nothing), we consider it as
    // having introduced a token onto the current line.
    lexed_tokens_on_line_ = true;
    return TOK_NONE;
  }

  TokenKind kind = pp_.findKeyword(atom);
  if (kind != TOK_NONE)
    return kind;

  if (matchChar(':'))
    return TOK_LABEL;
  return TOK_NAME;
}

TokenKind
Lexer::singleLineComment()
{
  while (!IsLineTerminator(peekChar()))
    skipChar();
  return TOK_COMMENT;
}

TokenKind
Lexer::multiLineComment(const SourceLocation& begin)
{
  while (true) {
    char c = readChar();
    if (c == '\r' || c == '\n') {
      advanceLine(c);
      continue;
    }

    if (c == '\0') {
      cc_.report(begin, rmsg::unterminated_comment);
      break;
    }

    if (c == '*') {
      if (matchChar('/'))
        break;
    }
  }
  return TOK_COMMENT;
}

// Advance line heuristics for newline character c.
void
Lexer::advanceLine(char c)
{
  assert(c == '\r' || c == '\n');
  if (c == '\r' && readChar() != '\n')
    putBack(c);

  line_number_++;
  lexed_tokens_on_line_ = false;
}

// Consume characters until we have something to start parsing from.
char
Lexer::consumeWhitespace()
{
  for (;;) {
    char c = readChar();
    switch (c) {
      case '\n':
      case '\r':
        if (lexing_for_directive_) {
          // Back up - don't consume the newline.
          putBack(c);
          return c;
        }

        advanceLine(c);
        break;

      case ' ':
      case '\t':
      case '\f':
        break;

      default:
        return c;
    }
  }
}

// Eat any trailing characters after a preprocessor directive, until we hit a
// newline. If we encountered no errors processing the directive, we usually
// want to throw an error if we see extra characters.
void
Lexer::chewLineAfterDirective(bool warnOnNonSpace)
{
  assert(lexing_for_directive_);

  SaveAndSet<bool> suppressErrors(&suppress_errors_, true);

  bool warned = false;
  for (;;) {
    Token tok;
    switch (directive_next(&tok)) {
      case TOK_EOL:
        return;
      case TOK_COMMENT:
        break;

      default:
        if (warnOnNonSpace && !warned) {
          // Note: use cc since we're suppressing internal errors.
          cc_.report(tok.start.loc, rmsg::pp_extra_characters);
          warned = true;
        }
        break;
    }
  }
}

void
Lexer::handleDirectiveWhileInactive()
{
  SaveAndSet<bool> inDirective(&lexing_for_directive_, true);
  SourceLocation begin = lastpos();
  TokenKind directive = maybeKeyword('#');
  switch (directive) {
    case TOK_M_IF:
    {
      // We need to push *something* here, otherwise we don't know which
      // #endifs match up to what.
      ifstack_.push_back(IfContext(begin, IfContext::Dead));
      break;
    }

    case TOK_M_ELSE:
    {
      // Only check and update the context if we're not inside a dead
      // context.
      IfContext* ix = currentIf();
      if (ix->state == IfContext::Dead)
        return;

      if (ix->elseloc.isSet()) {
        report(begin, rmsg::else_declared_twice)
          << cc_.note(ix->elseloc, rmsg::previous_location);
      }

      ix->elseloc = begin;
      if (ix->state == IfContext::Ignoring)
        ix->state = IfContext::Active;
      else
        ix->state = IfContext::Inactive;
      chewLineAfterDirective(true);
      break;
    }

    case TOK_M_ENDIF:
    {
      // We're guaranteed there's something pushed, since otherwise we wouldn't
      // be in handleIfContext().
      ifstack_.pop_back();
      chewLineAfterDirective(true);
      break;
    }

    default:
      // If we don't recognize the token, we just ignore it.
      break;
  }
}

void
Lexer::checkIfStackAtEndOfFile()
{
  if (IfContext* ix = currentIf()) {
    if (ix->elseloc.isSet())
      cc_.report(ix->elseloc, rmsg::unterminated_else);
    else
      cc_.report(ix->first, rmsg::unterminated_if);
  }

  // Clear so we don't report again.
  ifstack_.clear();
}

void
Lexer::handleIfContext()
{
  // Because we chew lines after a directive, we should be at a newline or
  // EOF right now.
  assert(peekChar() == '\r' ||
         peekChar() == '\n' ||
         peekChar() == '\0');
  if (peekChar() == '\0')
    return;
  advanceLine(peekChar());

  for (;;) {
    char c = firstNonSpaceChar();
    if (c == '#') {
      // Handle the directive. This might put us back into normal lexing
      // territory, so check afterward.
      handleDirectiveWhileInactive();
      if (!currentIf() || currentIf()->state == IfContext::Active)
        return;
    }

    while (!IsLineTerminator(c))
      c = readChar();

    if (c == '\0')
      return;
    advanceLine(c);
  }
}

TokenList*
Lexer::getMacroTokens()
{
  std::vector<Token> tokens;

  // We do not allow macro expansion while we're looking for tokens - we only
  // perform expansion during pasting.
  SaveAndSet<bool> disallowMacroExpansion(&pp_.macro_expansion(), false);

  for (;;) {
    Token tok;
    while (directive_next(&tok) == TOK_COMMENT)
      continue;
    if (tok.kind == TOK_EOL)
      break;
    tokens.push_back(tok);
  }

  TokenList* list = new (cc_.pool()) TokenList(tokens.size());
  for (size_t i = 0; i < tokens.size(); i++)
    list->at(i) = tokens[i];
  return list;
}

// Returns whether or not the user should be warned of trailing characters.
bool
Lexer::handlePreprocessorDirective()
{
  SourceLocation begin = lastpos();
  TokenKind directive = maybeKeyword('#');

  switch (directive) {
    case TOK_M_DEFINE:
    {
      Token tok;
      if (directive_next(&tok) != TOK_NAME) {
        cc_.report(tok.start.loc, rmsg::bad_directive_token)
          << TokenNames[TOK_NAME]
          << TokenNames[tok.kind];
        return false;
      }
      if (peekChar('(')) {
        report(pos(), rmsg::macro_functions_unsupported);
        return false;
      }

      // :TODO: do we want to track #defines for AST printing?

      TokenList* tokens = getMacroTokens();
      pp_.defineMacro(tok.atom(), tok.start.loc, tokens);
      return false;
    }

    case TOK_M_IF:
    {
      int val = 0;
      bool errored = pp_.eval(&val);
      
      ifstack_.push_back(IfContext(begin, val ? IfContext::Active : IfContext::Ignoring));
      return !errored;
    }

    case TOK_M_ELSE:
    {
      IfContext* ix = currentIf();
      if (!ix) {
        report(begin, rmsg::else_without_if);
        return false;
      }
      if (ix->elseloc.isSet()) {
        report(begin, rmsg::else_declared_twice)
          << cc_.note(begin, rmsg::previous_location);
        return false;
      }

      ix->elseloc = begin;
      if (ix->state == IfContext::Ignoring)
        ix->state = IfContext::Active;
      else
        ix->state = IfContext::Inactive;
      return true;
    }

    case TOK_M_ENDIF:
    {
      IfContext* ix = currentIf();
      if (!ix) {
        report(begin, rmsg::endif_without_if);
        return false;
      }

      ifstack_.pop_back();
      return true;
    }

    case TOK_M_UNDEF:
    {
      SaveAndSet<bool> disable_expansion(&pp_.macro_expansion(), false);
      Token tok;
      if (directive_next(&tok) != TOK_NAME) {
        cc_.report(tok.start.loc, rmsg::bad_directive_token)
          << TokenNames[TOK_NAME]
          << TokenNames[tok.kind];
        return false;
      }

      return pp_.removeMacro(tok.start.loc, tok.atom());
    }

    case TOK_M_ENDINPUT:
    {
      // Simulate reaching the end of the file.
      pos_ = end_;

      // Purge the ifstack, since the preprocessor will ask us to verify
      // whether or not we ended #if blocks.
      ifstack_.clear();
      return false;
    }

    case TOK_M_INCLUDE:
    case TOK_M_TRYINCLUDE:
    {
      // Search for a delimiter.
      char c = firstNonSpaceChar();
      if (c != '"' && c != '<') {
        report(lastpos(), rmsg::bad_include_syntax);
        return false;
      }

      char match = (c == '"') ? '"' : '>';

      literal_.clear();
      while (true) {
        if (IsLineTerminator(peekChar())) {
          report(lastpos(), rmsg::bad_include_syntax);
          return false;
        }

        char c = readChar();
        if (c == match)
          break;

        literal_.push_back(c);
      }
      literal_.push_back('\0');

      const char* where = nullptr;
      if (match == '"') {
        // We have to be in a file to be seeing #include.
        where = buffer_->path();
      }

      // Chew tokens beforehand, so we don't have to remember that we're in a
      // preprocessing state when we return to this buffer. For simplicity we
      // always warn here.
      chewLineAfterDirective(true);

      // We've already processed the rest of the line, so just hand control
      // back to the preprocessor where it can continue lexing (potentially
      // from a new file).
      pp_.enterFile(directive, begin, literal(), where);
      return false;
    }

    case TOK_M_PRAGMA:
    {
      Token tok;
      if (directive_next(&tok) != TOK_NAME) {
        cc_.report(tok.start.loc, rmsg::pragma_must_have_name);
        return false;
      }
      if (strcmp(tok.atom()->chars(), "deprecated") == 0) {
        const char* begin, *end;
        readUntilEnd(&begin, &end);

        pp_.setNextDeprecationMessage(begin, end - begin);
        return true;
      }
      if (strcmp(tok.atom()->chars(), "newdecls") == 0) {
        SaveAndSet<bool> disable_expansion(&pp_.macro_expansion(), false);

        // Whether or not newdecls are required is limited to the local lexer
        // options, though they are inherited.
        if (directive_next(&tok) != TOK_NAME) {
          cc_.report(tok.start.loc, rmsg::bad_pragma_newdecls);
          return false;
        }
        if (strcmp(tok.atom()->chars(), "required") == 0) {
          options_.RequireNewdecls = true;
          return true;
        }
        if (strcmp(tok.atom()->chars(), "optional") == 0) {
          options_.RequireNewdecls = false;
          return true;
        }
        cc_.report(tok.start.loc, rmsg::bad_pragma_newdecls);
        return false;
      }
      if (strcmp(tok.atom()->chars(), "semicolon") == 0) {
        // We ignore #pragma semicolon entirely now. There's a separate
        // top-level mode for users that wish to enforce it on their code.
        // Requiring it makes it harder to import someone else's code that
        // does not specify it, so as a language feature, it is now always-
        // optional.
        //
        // We still check that the directive is properly formed.
        int val = 0;
        return pp_.eval(&val);
      }
      if (strcmp(tok.atom()->chars(), "dynamic") == 0) {
        SourceLocation loc = tok.start.loc;

        int val = 0;
        if (!pp_.eval(&val))
          return false;

        ReportingContext rc(cc_, loc);
        return cc_.ChangePragmaDynamic(rc, val);
      }
      cc_.report(tok.start.loc, rmsg::unknown_pragma)
        << tok.atom();
      return false;
    }

    default:
      report(begin, rmsg::unknown_directive)
        << literal();
      return false;
  }
}

void
Lexer::enterPreprocessorDirective()
{
  // Note: it is very important we set this, since this will recursively
  // re-enter scan().
  lexed_tokens_on_line_ = true;

  {
    SaveAndSet<bool> setInDirective(&lexing_for_directive_, true);
    bool warnOnExtraChars = handlePreprocessorDirective();
    chewLineAfterDirective(warnOnExtraChars);
  }

  // If we are now in an inactive or ignored #if context, we sweep through
  // the file until we find a new position we can parse from.
  if (currentIf() && currentIf()->state != IfContext::Active) {
    handleIfContext();

    if (peekChar() == '\0') {
      // We reached the end of the file handling dead code. Just tell the
      // preprocessor (by returning out to it) so it can finish things up.
      return;
    }

    // We should be back into normal lexing now.
    assert(!currentIf() || currentIf()->state == IfContext::Active);
  }
}

TokenKind
Lexer::scan(Token* tok)
{
  char c = consumeWhitespace();

  // Preprocessor directives can only be parsed if they are the first token
  // on the line  and we're not already in a macro. SP1 allows preceding
  // comments (since it stripped them), as well as any amount of whitespace
  // to precede the directive. We allow that here as well.
  if (c == '#' && !lexed_tokens_on_line_) {
    // We don't give the preprocessor any token back, since it might want to
    // grab tokens from a new lexer.
    enterPreprocessorDirective();
    return TOK_NONE;
  }

  tok->init(TokenPos(lastpos(), line_number_), range_.id);

  TokenKind kind;
  switch (c) {
    case '\0':
      if (lexing_for_directive_) {
        kind = TOK_EOL;
        break;
      }

      checkIfStackAtEndOfFile();

      // Make sure we finish reading off |this| beforehand, since calling into
      // |pp| may destroy the lexer.
      tok->end = TokenPos(pos(), line_number_);
      if (pp_.handleEndOfFile())
        tok->kind = TOK_NONE;
      else
        tok->kind = TOK_EOF;
      return tok->kind;
    case ';':
      kind = TOK_SEMICOLON;
      break;
    case '{':
      kind = TOK_LBRACE;
      break;
    case '}':
      kind = TOK_RBRACE;
      break;
    case '(':
      kind = TOK_LPAREN;
      break;
    case ')':
      kind = TOK_RPAREN;
      break;
    case '[':
      kind = TOK_LBRACKET;
      break;
    case ']':
      kind = TOK_RBRACKET;
      break;
    case '~':
      kind = TOK_TILDE;
      break;
    case '?':
      kind = TOK_QMARK;
      break;
    case ':':
      kind = TOK_COLON;
      break;
    case ',':
      kind = TOK_COMMA;
      break;

    case '\r':
    case '\n':
      assert(lexing_for_directive_);
      kind = TOK_EOL;
      break;

    case '.':
      if (matchChar('.')) {
        if (matchChar('.')) {
          kind = TOK_ELLIPSES;
          break;
        }
        pos_ -= 1;
      }
      kind = TOK_DOT;
      break;

    case '/':
      if (matchChar('='))
        kind = TOK_ASSIGN_DIV;
      else if (matchChar('/'))
        kind = singleLineComment();
      else if (matchChar('*'))
        kind = multiLineComment(tok->start.loc);
      else
        kind = TOK_SLASH;
      break;

    case '*':
      if (matchChar('='))
        kind = TOK_ASSIGN_MUL;
      else
        kind = TOK_STAR;
      break;

    case '+':
      if (matchChar('='))
        kind = TOK_ASSIGN_ADD;
      else if (matchChar('+'))
        kind = TOK_INCREMENT;
      else
        kind = TOK_PLUS;
      break;

    case '&':
      if (matchChar('='))
        kind = TOK_ASSIGN_BITAND;
      else if (matchChar('&'))
        kind = TOK_AND;
      else
        kind = TOK_BITAND;
      break;

    case '|':
      if (matchChar('='))
        kind = TOK_ASSIGN_BITOR;
      else if (matchChar('|'))
        kind = TOK_OR;
      else
        kind = TOK_BITOR;
      break;

    case '^':
      if (matchChar('='))
        kind = TOK_ASSIGN_BITXOR;
      else
        kind = TOK_BITXOR;
      break;

    case '%':
      if (matchChar('='))
        kind = TOK_ASSIGN_MOD;
      else
        kind = TOK_PERCENT;
      break;

    case '-':
      if (matchChar('='))
        kind = TOK_ASSIGN_SUB;
      else if (matchChar('-'))
        kind = TOK_DECREMENT;
      else
        kind = TOK_MINUS;
      break;

    case '!':
      if (matchChar('='))
        kind = TOK_NOTEQUALS;
      else
        kind = TOK_NOT;
      break;

    case '=':
      if (matchChar('='))
        kind = TOK_EQUALS;
      else
        kind = TOK_ASSIGN;
      break;

    case '<':
      if (matchChar('=')) {
        kind = TOK_LE;
      } else if (matchChar('<')) {
        if (matchChar('='))
          kind = TOK_ASSIGN_SHL;
        else
          kind = TOK_SHL;
      } else {
        kind = TOK_LT;
      }
      break;

    case '>':
      if (matchChar('=')) {
        kind = TOK_GE;
      } else if (matchChar('>')) {
        if (matchChar('>')) {
          if (matchChar('='))
            kind = TOK_ASSIGN_USHR;
          else
            kind = TOK_USHR;
        } else {
          kind = TOK_SHR;
        }
      } else {
        kind = TOK_GT;
      }
      break;

    case '\'':
      kind = charLiteral(tok);
      break;

    case '"':
      kind = stringLiteral(tok);
      break;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      kind = handleNumber(tok, c);
      break;

    default:
      if (IsIdentStart(c)) {
        kind = handleIdentifier(tok, c);
        break;
      }

      // Don't report an error if we're lexing for a directive. We'll report it
      // later down the pipeline, rather than having the start of a valid token
      // that turns out to be deformed midway through.
      if (!lexing_for_directive_) {
        char print[2] = {c, '\0'};
        char code[16];
        SafeSprintf(code, sizeof(code), "%02X", uint8_t(c));

        cc_.report(tok->start.loc, rmsg::unexpected_char)
          << print << code;
      }
      kind = TOK_UNKNOWN;
      break;
  }

  if (kind != TOK_NONE && kind != TOK_COMMENT)
    lexed_tokens_on_line_ = true;

  tok->kind = kind;
  tok->end = TokenPos(pos(), line_number_);
  return kind;
}

// Lex for a token while inside a preprocessor directive. This is the same as
// next() but makes it clearer where we're coming from.
TokenKind
Lexer::directive_next(Token* tok)
{
  assert(lexing_for_directive_);

  // For now, we ignore comments completely while inside a macro.
  TokenKind kind;
  while ((kind = scan(tok)) == TOK_COMMENT)
  {}

  return kind;
}

// A front comment is a sequence of comments at most one line away from a non-
// comment token that is the first token on its line.
TokenKind
Lexer::processFrontCommentBlock(Token* tok)
{
  TokenPos start = tok->start;
  TokenPos end;

  TokenKind kind;
  TokenPos last_end = tok->end;
  while (true) {
    if ((kind = scan(tok)) != TOK_COMMENT) {
      // If we got something like this:
      //   /* ... */ status
      //
      // We do not consider this a front comment since it is ill style. We only
      // commit the last ending if the new token is on a different line.
      if (start.line == tok->start.line) {
        // Front comment should be discarded entirely, since the token was not
        // the first token on the line.
        return kind;
      }
      if (tok->start.line != last_end.line) {
        // The last comment ended on a different line from where this token
        // started, so we can commit that final comment.
        end = last_end;
      }
      break;
    }

    // Commit the last comment.
    end = last_end;

    // If this comment starts more than one line away from the previous ending,
    // we consider the comment block finished.
    if (tok->start.line > last_end.line + 1)
      break;

    last_end = tok->end;
  }

  // If we discarded all comments in the block, this will be empty.
  if (!end.loc.isSet())
    return kind;

  pp_.addComment(CommentPos::Front, SourceRange(start.loc, end.loc));
  return kind;
}

// A tail comment is a sequence of comments appearing after a token, ending
// after a blank line or a non-comment token.
TokenKind
Lexer::processTailCommentBlock(Token* tok)
{
  TokenPos start = tok->start;
  TokenPos end = tok->end;

  TokenKind kind;
  while ((kind = scan(tok)) == TOK_COMMENT) {
    if (tok->start.line > end.line + 1)
      break;
    end = tok->end;
  }

  pp_.addComment(CommentPos::Tail, SourceRange(start.loc, end.loc));
  return kind;
}

// Note: this calls back into scan(), so we should only call it from next().
TokenKind
Lexer::handleComments(Token* tok)
{
  // We don't bother inserting comments from macros, or if we're not parsing
  // for an AST dump.
  TokenKind kind = tok->kind;
  if (!pp_.traceComments() || lexing_for_directive_) {
    while ((kind = scan(tok)) == TOK_COMMENT)
    {}
    return kind;
  }

  if (lexed_tokens_on_line_)
    kind = processTailCommentBlock(tok);

  // If we get another comment or still have our original comment then our
  // next token should be on its own line.
  if (kind == TOK_COMMENT)
    assert(!lexed_tokens_on_line_);

  // We can have multiple front comment blocks.
  while (kind == TOK_COMMENT)
    kind = processFrontCommentBlock(tok);
  return kind;
}

TokenKind
Lexer::next(Token* tok)
{
  TokenKind kind = scan(tok);
  if (kind == TOK_COMMENT) {
    kind = handleComments(tok);
    assert(kind != TOK_COMMENT);
  }

  // This can differ if we return TOK_NONE. We do this since we don't want to
  // accidentally destroy part of the token ring in the preprocessor just to
  // enter into a preprocessor directive.
  assert(kind == TOK_NONE || kind == tok->kind);
  return kind;
}
