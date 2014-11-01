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
#ifndef _include_jitcraft_compile_context_h_
#define _include_jitcraft_compile_context_h_

#include <am-vector.h>
#include <am-hashtable.h>
#include <am-threadlocal.h>
#include "pool-allocator.h"
#include "auto-string.h"
#include "messages.h"
#include "tokens.h"
#include "string-pool.h"
#include <stdarg.h>
#include <string.h>
#include "type-manager.h"

namespace ke {

class ParseTree;
class GlobalScope;

class FileContext
{
 public:
  FileContext(FileContext *parent, const char *path)
   : path_(path),
     parent_(parent)
  {
  }

  const char *path() const {
    return path_;
  }
  FileContext *parent() const {
    return parent_;
  }

 private:
  AutoString path_;
  FileContext *parent_;
};

class TranslationUnit : public FileContext
{
 public:
  TranslationUnit(const char *path, char *text, size_t length)
   : FileContext(nullptr, path),
     text_(text),
     length_(length),
     tree_(nullptr),
     globalScope_(nullptr)
  {
  }

  ~TranslationUnit()
  {
    free(text_);
    for (size_t i = 0; i < files_.length(); i++)
      delete files_[i];
  }

  char *text() const {
    return text_;
  }
  size_t length() const {
    return length_;
  }
  FileContext *fileAt(size_t index) {
    return files_[index];
  }
  void attach(ParseTree *tree) {
    tree_ = tree;
  }
  ParseTree *tree() const {
    return tree_;
  }
  GlobalScope *globalScope() const {
    return globalScope_;
  }
  void setGlobalScope(GlobalScope *scope) {
    globalScope_ = scope;
  }

  void givePreprocessedText(char *text, size_t length, Vector<FileContext *> &files) {
    free(text_);
    text_ = text;
    length_ = length;
    files_ = ke::Move(files);
  }

 private:
  char *text_;
  size_t length_;
  Vector<FileContext *> files_;
  ParseTree *tree_;
  GlobalScope *globalScope_;
};

struct CompileError
{
  CompileError()
  : message(nullptr)
  {
  }
  CompileError(FileContext *file, unsigned line, unsigned col, const char *type, char *message)
  : file(file),
    line(line),
    col(col),
    type(type),
    message(message)
  {
  }

  FileContext *file;
  unsigned line;
  unsigned col;
  const char *type;
  char *message;
};

class CompileContext
{
 public:
  CompileContext(int argc, char **argv);
  ~CompileContext();

  bool compile();

  size_t nerrors() const {
    return errors_.length();
  }
  const CompileError &getError(size_t i) {
    return errors_[i];
  }
  bool outOfMemory() {
    return outOfMemory_;
  }

  PoolAllocator &pool() {
    return pool_;
  }

  TypeManager *types() {
    return &types_;
  }

  // String interning.
  Atom *add(const char *str) {
    return strings_.add(str);
  }
  Atom *add(const char *str, size_t length) {
    return strings_.add(str, length);
  }

  TokenKind findKeyword(const char *id);

  void reportErrorVa(const SourceLocation &loc, Message msg, va_list ap);
  void reportError(const SourceLocation &loc, Message msg, ...);
  void setOutOfMemory() {
    outOfMemory_ = true;
  }

  size_t numSearchPaths() const {
    return searchPaths_.length();
  }
  const AutoString &getSearchPath(size_t i) {
    return searchPaths_[i];
  }

 private:
  bool initKeywords();
  bool defineKeyword(TokenKind tok);

 private:
  struct KeywordEntry {
    const char *id;
    TokenKind tok;
  };

  struct KeywordPolicy {
    typedef KeywordEntry Payload;

    static uint32_t hash(const char *key) {
    return HashCharSequence(key, strlen(key));
    }

    static bool matches(const char *key, const KeywordEntry &e) {
    return strcmp(key, e.id) == 0;
    }
  };

  typedef HashTable<KeywordPolicy> KeywordTable;
  
 private:
  bool outOfMemory_;
  PoolAllocator pool_;
  Vector<TranslationUnit *> units_;
  Vector<CompileError> errors_;
  KeywordTable keywords_;
  StringPool strings_;
  Vector<AutoString> searchPaths_;
  TypeManager types_;
};

bool ReadFileChars(const char *path, char **textp, size_t *lengthp);

extern ke::ThreadLocal<CompileContext *> CurrentCompileContext;

static inline PoolAllocator &POOL()
{
  return CurrentCompileContext->pool();
}

} // namespace ke

#endif // _include_jitcraft_compile_context_h_

