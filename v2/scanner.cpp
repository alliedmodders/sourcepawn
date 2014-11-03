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
#include "scanner.h"
#include <assert.h>
#include <ctype.h>
#include <math.h>
#include <limits.h>
#include <string.h>

using namespace ke;

const char *ke::TokenNames[] =
{
#define _(name, str) str,
  TOKENMAP(_)
#undef _
  nullptr
};

Scanner::Scanner(CompileContext &cc, TranslationUnit *tu)
  : BasicLexer(cc, tu, tu->text(), tu->length()),
    tu_(tu),
    allowTags_(true),
    cursor_(0),
    depth_(0),
    require_semicolons_(false),
    require_newdecls_(false)
{
}

// Based off the logic in sc2.c's ftoi()...
double
Scanner::ParseDouble(const char *string)
{
  const char *ptr = string;

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

// Based on the logic for litchar() in sc2.c.
int
Scanner::readEscapeCode()
{
  char c = read();
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
  
      c = read();
      while (IsHexDigit(c) && digits < 2) {
        if (IsDigit(c))
          c = (c << 4) + (c - '0');
        else
          c = (c << 4) + (tolower(c) - 'a' + 10);
        digits++;
        c = read();
       }
        
      // Swallow a trailing ';'
      if (c != ';')
        pos_--;
  
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
          c = read();
        }

        // Swallow a trailing ;
        if (c != ';')
          pos_--;

        return r;
      }
      cc_.reportError(begin(), Message_UnknownEscapeCode, c);
      return INT_MAX;
    }
  }
}

bool
Scanner::charLiteral(Token *tok)
{
  tok->kind = TOK_CHAR_LITERAL;

  char c = read();
  if (c == '\'') {
    cc_.reportError(begin(), Message_InvalidCharLit);
    return true;
  }

  if (c == '\\')
    tok->setCharValue(readEscapeCode());
  else
    tok->setCharValue(c);

  c = read();
  if (c != '\'') {
    cc_.reportError(begin(), Message_MismatchedCharTerminator);

    // If the user did something like '5", assume it was a typo and keep the
    // token. Otherwise, backtrack.
    if (c != '"')
      pos_--;
  }

  return true;
}

bool
Scanner::stringLiteral(Token *tok)
{
  literal_.clear();

  for (;;) {
    char c = read();
    if (c == '\"')
      break;
    if (c == '\r' || c == '\n' || c == '\0') {
      cc_.reportError(begin(), Message_UnterminatedString);
      return false;
    }
    if (c == '\\') {
      int code = readEscapeCode();
      if (code == INT_MAX)
        return false;
      c = char(code);
    }
    if (!literal_.append(c))
      return false;
  }

  if (!literal_.append('\0'))
    return false;

  Atom *atom = cc_.add(BasicLexer::literal());
  if (!atom)
    return false;

  tok->kind = TOK_STRING_LITERAL;
  tok->setAtom(atom);
  return true;
}

void
Scanner::identifier(Token *tok, char first)
{
  literal_.clear();

  tok->kind = BasicLexer::identifier(first);
  if (tok->kind != TOK_NAME)
    return;

  if (allowTags_ && matchChar(':'))
    tok->kind = TOK_LABEL;

  tok->setAtom(BasicLexer::name());
}

void
Scanner::readDirective()
{
  literal_.clear();

  TokenKind tok = BasicLexer::identifier('#');
  switch (tok) {
    case TOK_M_FILE:
    {
#ifndef NDEBUG
      TokenKind tok = 
#endif
        next();
      assert(tok == TOK_INTEGER_LITERAL);

      int index = current()->int32Value();

      // Note: it is very dangerous to lex outside of the normal stream. In
      // spcomp1, we used two separate token buffers to prevent preproc tokens
      // from getting into the outer lexer. Here, so far, this is the only
      // edge case, so we just roll back the cursor.
      cursorPrev();

      enter(tu_->fileAt(index));
      break;
    }

    case TOK_M_DEPRECATE:
    {
      // Eat whitespace that doesn't end the line.
      while (IsSkipSpace(get()))
        read();

      const char *begin = pos_;
      while (!IsLineTerminator(get()))
        read();
      const char *end = pos_;

      // Give the newline back.
      pos_--;

      AString message(begin, end - begin);

      pending_deprecate_ = true;
      deprecation_message_ = ke::Move(message);
      break;
    }

    case TOK_M_LEAVING:
      leave();
      break;

    case TOK_M_REQUIRE_SEMI:
      require_semicolons_ = true;
      break;
    case TOK_M_OPTIONAL_SEMI:
      require_semicolons_ = false;
      break;
    case TOK_M_REQUIRE_NEWDECLS:
      require_newdecls_ = true;
      break;
    case TOK_M_OPTIONAL_NEWDECLS:
      require_newdecls_ = false;
      break;

    default:
      assert(false);
  }

  // scan until end of line.
  while (!IsLineTerminator(get()))
    read();

  // If we got a TOK_M_FILE, read past the newline so we don't add an extra
  // line erroneously to the line count.
  if (tok == TOK_M_FILE) {
    assert(get() == '\n');
    read();
  }
}

bool
Scanner::eatWhitespace()
{
  for (;;) {
    char c = read();
    switch (c) {
      case '\n':
      case '\r':
        nextline(c);
        break;

      case ' ':
      case '\t':
      case '\f':
        break;

      case '#':
        if (pos_ != linebegin_ + 1) {
          // Fudge up a token since scan() hasn't set everything up yet.
          pos_--;
          current()->start = SourceLocation(file(), pos());
          pos_++;
          current()->end = SourceLocation(file(), pos());
          current()->kind = TOK_ERROR;

          cc_.reportError(current()->start, Message_UnexpectedCharacter, c, uint8_t(c));
          return false;
        }
        readDirective();
        break;

      case '\0':
        // Don't back up, we want an EOF.
        return true;

      default:
        pos_--;
        return true;
    }
  }
}

TokenKind
Scanner::scan()
{
  assert(depth_ == 0);

  Token *tok = cursorNext();
  tok->setAtom(nullptr);

  if (!eatWhitespace()) {
    return TOK_ERROR;
  }

  tok->start = SourceLocation(file(), pos());

  char c = read();
  switch (c) {
    case '\0':
      tok->kind = TOK_EOF;
      break;
    case ';':
      tok->kind = TOK_SEMICOLON;
      break;
    case '{':
      tok->kind = TOK_LBRACE;
      break;
    case '}':
      tok->kind = TOK_RBRACE;
      break;
    case '(':
      tok->kind = TOK_LPAREN;
      break;
    case ')':
      tok->kind = TOK_RPAREN;
      break;
    case '[':
      tok->kind = TOK_LBRACKET;
      break;
    case ']':
      tok->kind = TOK_RBRACKET;
      break;
    case '~':
      tok->kind = TOK_TILDE;
      break;
    case '?':
      tok->kind = TOK_QMARK;
      break;
    case ':':
      tok->kind = TOK_COLON;
      break;
    case ',':
      tok->kind = TOK_COMMA;
      break;

    case '.':
      tok->kind = TOK_DOT;
      if (matchChar('.')) {
        if (matchChar('.'))
          tok->kind = TOK_ELLIPSES;
        else
          pos_ -= 2;
      }
      break;

    case '/':
      if (matchChar('='))
        tok->kind = TOK_ASSIGN_DIV;
      else
        tok->kind = TOK_SLASH;
      break;

    case '*':
      if (matchChar('='))
        tok->kind = TOK_ASSIGN_MUL;
      else
        tok->kind = TOK_STAR;

    case '+':
      if (matchChar('='))
        tok->kind = TOK_ASSIGN_ADD;
      else if (matchChar('+'))
        tok->kind = TOK_INCREMENT;
      else
        tok->kind = TOK_PLUS;
      break;

    case '&':
      if (matchChar('='))
        tok->kind = TOK_ASSIGN_BITAND;
      else if (matchChar('&'))
        tok->kind = TOK_AND;
      else
        tok->kind = TOK_BITAND;
      break;

    case '|':
      if (matchChar('='))
        tok->kind = TOK_ASSIGN_BITOR;
      else if (matchChar('|'))
        tok->kind = TOK_OR;
      else
        tok->kind = TOK_BITOR;
      break;

    case '^':
      if (matchChar('='))
        tok->kind = TOK_ASSIGN_BITXOR;
      else
        tok->kind = TOK_BITXOR;
      break;

    case '%':
      if (matchChar('='))
        tok->kind = TOK_ASSIGN_MOD;
      else
        tok->kind = TOK_PERCENT;
      break;

    case '-':
      if (matchChar('='))
        tok->kind = TOK_ASSIGN_SUB;
      else if (matchChar('-'))
        tok->kind = TOK_DECREMENT;
      else
        tok->kind = TOK_MINUS;
      break;

    case '!':
      if (matchChar('='))
        tok->kind = TOK_NOTEQUALS;
      else
        tok->kind = TOK_NOT;
      break;

    case '=':
      if (matchChar('='))
        tok->kind = TOK_EQUALS;
      else
        tok->kind = TOK_ASSIGN;
      break;

    case '<':
      if (matchChar('=')) {
        tok->kind = TOK_LE;
      } else if (matchChar('<')) {
        if (matchChar('='))
          tok->kind = TOK_ASSIGN_SHL;
        else
          tok->kind = TOK_SHL;
      } else {
        tok->kind = TOK_LT;
      }
      break;

    case '>':
      if (matchChar('=')) {
        tok->kind = TOK_GE;
      } else if (matchChar('>')) {
        if (matchChar('>')) {
          if (matchChar('='))
            tok->kind = TOK_ASSIGN_USHR;
          else
            tok->kind = TOK_USHR;
        } else {
          tok->kind = TOK_SHR;
        }
      } else {
        tok->kind = TOK_GT;
      }
      break;

    case '\'':
      if (!charLiteral(tok))
        tok->kind = TOK_ERROR;
      break;

    case '"':
      if (!stringLiteral(tok))
        tok->kind = TOK_ERROR;
      break;

    default:
      if (c >= '0' && c <= '9') {
        numberLiteral(tok, c);
      } else if (c == '_' || (isascii(c) && isalpha(c))) {
        identifier(tok, c);
      } else {
        cc_.reportError(begin(), Message_UnexpectedCharacter, c, uint8_t(c));
        tok->kind = TOK_ERROR;
      }
      break;
  }

  tok->end = SourceLocation(file(), pos());
  return tok->kind;
}

void
Scanner::numberLiteral(Token *tok, char c)
{
  // :TODO: we need to warn/error here if values are out of any conceivable range.
  tok->kind = BasicLexer::numberLiteral(c);
  switch (tok->kind) {
    case TOK_INTEGER_LITERAL:
      tok->setIntValue(strtol(BasicLexer::literal(), nullptr, 10));
      break;
    case TOK_HEX_LITERAL:
      tok->setIntValue(strtol(BasicLexer::literal(), nullptr, 16));
      break;
    case TOK_FLOAT_LITERAL:
      tok->setDoubleValue(ParseDouble(BasicLexer::literal()));
      break;
    case TOK_ERROR:
      break;
    default:
      assert(false);
      break;
  }
}

TokenKind
Scanner::next()
{
  if (depth_ > 0)
    cursorPop();
  else
    scan();
  return current()->kind;
}

TokenKind
Scanner::peek()
{
  if (depth_ == 0) {
    scan();
    undo();
  }
  return cursorPeek()->kind;
}

// Move the cursor to the next token slot.
Token *
Scanner::cursorNext()
{
  assert(depth_ == 0);

  num_tokens_++;
  cursor_++;
  if (cursor_ == kMaxLookahead)
    cursor_ = 0;

  return current();
}

// Peek at the next buffered token.
Token *
Scanner::cursorPeek()
{
  assert(depth_ > 0);
  int cursor = cursor_ + 1;
  if (cursor == kMaxLookahead)
    cursor = 0;
  return &tokens_[cursor];
}

// Move the cursor to the previous token slot.
void
Scanner::cursorPrev()
{
  if (cursor_ == 0)
    cursor_ = kMaxLookahead - 1;
  else
    cursor_--;
}

// Skip past the most recently buffered token.
void
Scanner::cursorPop()
{
  assert(depth_ > 0);
  depth_--;
  cursor_++;
  if (cursor_ == kMaxLookahead)
    cursor_ = 0;
}

void
Scanner::undo()
{
  assert(depth_ < kMaxLookahead);
  depth_++;
  cursorPrev();
  assert(depth_ <= num_tokens_);
}

// This function destroys the potential lookahead buffer by injecting a token
// that will be seen by the following call to next(). Calls to undo() are
// preserved, but further calls to undo() will not be able to look past the
// injected token.
void
Scanner::pushBack(const Token &tok)
{
  if (depth_ == 0) {
    Token *ptr = cursorNext();
    *ptr = tok;
    undo();
  } else {
    *current() = tok;
    undo();
  }
}

TokenKind
Scanner::peekTokenSameLine()
{
  // We should not call this without having parsed at least one token.
  assert(num_tokens_ > 0);

  // If there's tokens pushed back, then |fline| is the line of the furthest
  // token parsed. If fline == current token's line, we are guaranteed any
  // buffered token is still on the same line.
  if (depth_ > 0 && current()->end.line == line_)
    return cursorPeek()->kind;

  // Make sure the next token is lexed, then buffer it.
  if (depth_ == 0) {
    scan();
    undo();
  }

  Token *next = cursorPeek();

  // If the next token starts on the line the last token ends at, then the
  // next token is considered on the same line.
  if (next->start.line == current()->end.line)
    return next->kind;

  return TOK_EOL;
}

void
Scanner::eatRestOfLine()
{
  while (!IsLineTerminator(get()))
    read();
}
