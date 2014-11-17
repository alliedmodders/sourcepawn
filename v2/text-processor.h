/* vim: set ts=2 sw=2 tw=99 et:
 *
 * Copyright (C) 2012-2013 David Anderson
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
#ifndef _include_sourcepawn_text_processing_h_
#define _include_sourcepawn_text_processing_h_

#include "tokens.h"
#include <am-vector.h>
#include "compile-context.h"

namespace ke {

class TextProcessor
{
 public:
  TextProcessor(char *stream, size_t length)
  : stream_(stream),
    pos_(stream),
    end_(stream + length),
    linebegin_(stream),
    line_(1)
  {
  }

  TextProcessor(char *stream, char *end) 
  : stream_(stream),
    pos_(stream),
    end_(end),
    linebegin_(stream),
    line_(1)
  {
  }

  char *ptr() const {
    return pos_;
  }
  char get() {
    if (!canRead())
      return '\0';
    return *pos_;
  }
  char read() {
    if (!canRead())
      return '\0';
    return *pos_++;
  }
  bool peekChar(char c) {
    bool found = (read() == c);
    pos_--;
    return found;
  }
  bool matchChar(char c) {
    if (read() == c)
      return true;
    pos_--;
    return false;
  }

  void nextline(char c) {
    if (c == '\r' && read() != '\n')
      pos_--;
    linebegin_ = pos_;
    line_++;
  }

  SourceLocation pos() const {
    // :SRCLOC:
    return SourceLocation();
  }
  SourceLocation begin() const {
    // :SRCLOC:
    return SourceLocation();
  }

  static inline bool IsDigit(char c) {
    return c >= '0' && c <= '9';
  }
  static inline bool IsHexDigit(char c) {
    return IsDigit(c) ||
         (c >= 'a' && c <= 'f') ||
         (c >= 'A' && c <= 'F');
  }
  static inline bool IsLineTerminator(char c) {
    return c == '\n' || c == '\r' || c == '\0';
  }
  static inline bool IsSkipSpace(char c) {
    return c == ' ' || c == '\t';
  }
  static inline bool IsIdentStart(char c) {
    return (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z') ||
            c == '_';
  }
  static inline bool IsIdentChar(char c) {
    return IsIdentStart(c) || (c >= '0' && c <= '9');
  }

  // read() will return the first non-space char.
  char *skipSpaces() {
    while (IsSkipSpace(get()))
      read();
    return ptr();
  }
  // Same as skipSpaces(), but performs a final read().
  char firstNonSpaceChar() {
    char c = read();
    while (IsSkipSpace(c))
      c = read();
    return c;
  }

  bool more() {
    return canRead();
  }
  bool stacked() const {
    return !saved_streams_.empty();
  }

  // In order to make macro substitution work sort of sanely, we allow a
  // single lexer to nest streams.
  void addText(char *text, size_t length) {
    SavedStream saved;
    saved.stream = stream_;
    saved.pos = pos_;
    saved.end = end_;
    saved.linebegin = linebegin_;
    saved_streams_.append(saved);

    stream_ = text;
    pos_ = text;
    end_ = text + length;
    linebegin_ = text;
  }

 protected:
  bool canRead() {
    if (pos_ < end_)
      return true;
    while (pos_ >= end_) {
      if (saved_streams_.empty())
        return false;
      stream_ = saved_streams_.back().stream;
      pos_ = saved_streams_.back().pos;
      end_ = saved_streams_.back().end;
      linebegin_ = saved_streams_.back().linebegin;
      saved_streams_.pop();
    }
    return true;
  }

 protected:
  struct SavedStream {
    char *stream;
    char *pos;
    char *end;
    char *linebegin;
  };

 protected:
  char *stream_;
  char *pos_;
  char *end_;
  char *linebegin_;
  char *begin_;
  size_t line_;
  Vector<SavedStream> saved_streams_;
};

class BasicLexer : public TextProcessor
{
 public:
  BasicLexer(CompileContext &cc, FileContext *file, char *start, size_t length)
   : TextProcessor(start, length),
     cc_(cc),
     file_(file)
  {
  }

  const char *literal() const {
    return literal_.buffer();
  }
  size_t literal_length() const {
    return literal_.length() - 1;
  }

  Atom *name() const {
    return cc_.add(literal());
  }

  FileContext *file() const {
    return file_;
  }
  SourceLocation loc() const {
    // :SRCLOC:
    return SourceLocation();
  }
  // Beginning source position of the last instruction.
  SourceLocation begin() const {
    // :SRCLOC:
    return SourceLocation();
  }

 protected:
  TokenKind hexLiteral() {
    literal_.clear();

    for (;;) {
      char c = read();
      if (!IsHexDigit(c)) {
        pos_--;
        break;
      }
      literal_.append(c);
    }

    literal_.append('\0');
    return TOK_HEX_LITERAL;
  }

  TokenKind numberLiteral(char first) {
    literal_.clear();
    literal_.append(first);

    char c;
    for (;;) {
      c = read();
      if (!IsDigit(c))
        break;
      literal_.append(c);
    }

    // Detect a hexadecimal string.
    if (literal_.length() == 1 &&
        literal_[0] == '0' &&
        (c == 'x' || c == 'X'))
    {
      return hexLiteral();
    }

    if (c != '.') {
      pos_--;
      literal_.append('\0');
      return TOK_INTEGER_LITERAL;
    }
    literal_.append(c);

    c = read();
    if (!IsDigit(c)) {
      cc_.reportError(begin(), Message_ExpectedDigitForFloat, c);
      return TOK_ERROR;
    }
    literal_.append(c);

    for (;;) {
      c = read();
      if (!IsDigit(c))
        break;
      literal_.append(c);
    }

    if (c != 'e') {
      pos_--;
    } else {
      literal_.append(c);
      c = read();
      if (c == '-') {
        literal_.append(c);
        c = read();
      }
      if (!IsDigit(c)) {
        cc_.reportError(begin(), Message_ExpectedDigitForFloat, c);
        return TOK_ERROR;
      }
      literal_.append(c);
      for (;;) {
        c = read();
        if (!IsDigit(c)) {
          pos_--;
          break;
        }
        literal_.append(c);
      }
    }

    literal_.append('\0');
    return TOK_FLOAT_LITERAL;
  }

  TokenKind name(char first) {
    literal_.append(first);

    char c;
    for (;;) {
      c = read();
      if (!IsIdentChar(c)) {
        pos_--;
        break;
      }
      literal_.append(c);
    }
    literal_.append('\0');
    return TOK_NAME;
  }

  TokenKind identifier(char first) {
    name(first);

    // Most keywords do not act as labels.
    TokenKind kind = cc_.findKeyword(literal());
    if (kind != TOK_NONE)
      return kind;
    return TOK_NAME;
  }

  void enter(FileContext *file) {
    files_.append(FileAndLine(file_, line_));
    file_ = file;
    line_ = 1;
  }
  void leave() {
    file_ = files_.back().file;
    line_ = files_.back().line;
    files_.pop();
  }

 protected:
  struct FileAndLine {
    FileContext *file;
    size_t line;

    FileAndLine()
    { }
    FileAndLine(FileContext *file, size_t line)
      : file(file), line(line)
    { }
  };

  CompileContext &cc_;
  FileContext *file_;
  Vector<FileAndLine> files_;
  Vector<char> literal_;
};

static inline int
StringToInt32(const char *ptr)
{
  int v = 0;
  while (TextProcessor::IsDigit(*ptr) || *ptr == '_') {
    if (*ptr != '_')
      v = (v * 10) + (*ptr - '0');
    ptr++;
  }
  return v;
}

} // namespace ke

#endif // _include_sourcepawn_text_processing_h_

