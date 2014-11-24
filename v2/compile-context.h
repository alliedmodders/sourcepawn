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
#include <stdarg.h>
#include <string.h>
#include "pool-allocator.h"
#include "auto-string.h"
#include "messages.h"
#include "tokens.h"
#include "string-pool.h"
#include "type-manager.h"
#include "process-options.h"

namespace ke {

class ParseTree;
class GlobalScope;
class SourceManager;

class TranslationUnit : public PoolObject
{
 public:
  TranslationUnit()
   : tree_(nullptr),
     globalScope_(nullptr)
  {
  }

  ~TranslationUnit()
  {
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
  void attach(ParseTree *tree) {
    assert(!tree_);
    tree_ = tree;
  }

 private:
  ParseTree *tree_;
  GlobalScope *globalScope_;
};

struct CompileError
{
  CompileError()
  : message(nullptr)
  {
  }
  CompileError(const SourceLocation &loc, const char *type, char *message)
   : loc(loc),
     type(type),
     message(message)
  {
  }

  SourceLocation loc;
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
  SourceManager &source() {
    return **source_;
  }

  // String interning.
  Atom *add(const char *str) {
    return strings_.add(str);
  }
  Atom *add(const char *str, size_t length) {
    return strings_.add(str, length);
  }

  // Option changing.
  bool ChangePragmaDynamic(ReportingContext &rc, int64_t value);

  void reportErrorVa(const SourceLocation &loc, Message msg, va_list ap);
  void reportError(const SourceLocation &loc, Message msg, ...);

  Atom *createAnonymousName(const SourceLocation &loc);

 private:
  bool outOfMemory_;
  PoolAllocator pool_;
  AutoPtr<SourceManager> source_;
  Vector<CompileError> errors_;
  StringPool strings_;
  TypeManager types_;
  CompileOptions options_;
};

bool ReadFileChars(const char *path, char **textp, size_t *lengthp);

extern ke::ThreadLocal<CompileContext *> CurrentCompileContext;

static inline PoolAllocator &POOL()
{
  return CurrentCompileContext->pool();
}

struct ReportingContext
{
 public:
  ReportingContext(CompileContext &cc, const SourceLocation &loc, bool shouldError = true)
   : cc_(cc),
     loc_(loc),
     should_error_(shouldError)
  {}

  void reportError(Message msg, ...);

 private:
  CompileContext &cc_;
  const SourceLocation &loc_;
  bool should_error_;
};

} // namespace ke

#endif // _include_jitcraft_compile_context_h_

