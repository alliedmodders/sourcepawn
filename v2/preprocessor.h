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
#ifndef _include_sourcepawn_preprocessor_h_
#define _include_sourcepawn_preprocessor_h_

#include "compile-context.h"
#include "compile-buffer.h"
#include "text-processor.h"
#include "auto-string.h"

namespace ke {

enum IfState {
  IfNone,
  IfReading,      // "true" section of an #if/#else chain
  IfIgnoring,     // "false" section of an #if/#else chain
  IfMustIgnore,   // "true" already seen, so always ignore.
};

class PreprocessingLexer : public BasicLexer
{
 public:
  PreprocessingLexer(CompileContext &cc, FileContext *file, char *text, size_t length);

  TokenKind command();
  TokenKind lex();
  TokenKind peek();
  bool match(TokenKind kind) {
    if (peek() != kind)
      return false;
    pushed_ = false;
    return true;
  }
  bool expect(TokenKind kind);
  void readline();
  bool readEndOfDirective();
  void readUntilEnd(char **begin, char **end);
  TokenKind readUntilName(CompileBuffer &buffer, bool *tokenWasStacked);

  // #if, #else, #endif helpers.
  void pushif(const SourceLocation &pos, IfState state) {
    ifstack_.append(IfEntry(pos, state));
  }
  void popif() {
    ifstack_.pop();
  }
  IfState ifstate() const {
    return ifstack_.empty()
           ? IfNone
           : ifstack_.back().state;
  }
  bool got_else() const {
    assert(!ifstack_.empty());
    return ifstack_.back().got_else;
  }
  void set_in_else() {
    assert(!ifstack_.empty());
    ifstack_.back().got_else = true;
    if (ifstate() == IfReading)
      ifstack_.back().state = IfIgnoring;
    else
      ifstack_.back().state = IfReading;
  }
  SourceLocation ifpos() const {
    // :SRCLOC:
    return SourceLocation();
  }

  FileContext *file() const {
    return file_;
  }

  void endinput() {
    assert(!stacked());
    pos_ = end_;
  }
  void purgeIfStack() {
    ifstack_.clear();
  }

  void resetToLineStart() {
    pos_ = linebegin_;
  }

 private:
  TokenKind scan();

 private:
  struct IfEntry {
    SourceLocation pos;
    IfState state;
    bool got_else;
    IfEntry() : state(IfNone)
    { }
    IfEntry(const SourceLocation &pos, IfState state)
      : pos(pos), state(state), got_else(false)
    { }
  };

  TokenKind saved_;
  bool pushed_;
  Vector<IfEntry> ifstack_;
};

class Preprocessor
{
 public:
  Preprocessor(CompileContext &cc);
  ~Preprocessor();

  void preprocess(TranslationUnit *unit);

 private:
  bool expr(int *val);
  bool unary(int *val);
  bool binary(int prec, int *val);

  AutoString searchPaths(const char *file);

  bool include(TokenKind cmd, const char *file);
  bool directive(TokenKind cmd);
  void substitute();
  void check_ignored_command(TokenKind cmd);
  void preprocess();
  void preprocess(FileContext *file, char *text, size_t length);
  void setup_global_macros();

 private:
  struct MacroEntry {
    Atom *id;
    char *value;
    size_t valueLength;
    FileContext *file;
    SourceLocation loc;

    MacroEntry() { }
    MacroEntry(Atom *id, char *value, size_t length,
               FileContext *file, const SourceLocation &loc)
    : id(id),
      value(value),
      valueLength(length),
      file(file),
      loc(loc)
    {
    }
  };

  struct MacroPolicy {
    typedef MacroEntry Payload;

    static uint32_t hash(Atom *key) {
      return HashPointer((void *)key);
    }

    static bool matches(Atom *key, const Payload &other) {
      return key == other.id;
    }
  };

  typedef HashTable<MacroPolicy, SystemAllocatorPolicy> MacroTable;

  CompileContext &cc_;
  CompileBuffer buffer_;
  PreprocessingLexer *text_;
  Vector<PreprocessingLexer *> lexers_;

  Vector<char> command_;
  MacroTable macros_;
  StringPool macroStrings_;

  Vector<FileContext *> files_;

  char date_[64];
  char ltime_[64];
};

}

#endif // _include_sourcepawn_preprocessor_h_

