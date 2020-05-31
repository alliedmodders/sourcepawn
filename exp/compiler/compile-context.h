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

#include <stdarg.h>
#include <string.h>

#include <amtl/am-hashtable.h>
#include <amtl/am-threadlocal.h>
#include <amtl/am-vector.h>

#include "shared/string-pool.h"
#include "pool-allocator.h"
#include "auto-string.h"
#include "type-manager.h"
#include "process-options.h"
#include "reporting.h"

namespace sp {

class GlobalScope;
class SourceFile;
class SourceManager;

namespace ast {
class ParseTree;
} // namespace ast

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

  ast::ParseTree* tree() const {
    return tree_;
  }
  GlobalScope* globalScope() const {
    return globalScope_;
  }
  void setGlobalScope(GlobalScope* scope) {
    globalScope_ = scope;
  }
  void attach(ast::ParseTree* tree) {
    assert(!tree_);
    tree_ = tree;
  }

 private:
  ast::ParseTree* tree_;
  GlobalScope* globalScope_;
};

class CompileContext
{
 public:
  CompileContext(PoolAllocator& pool, StringPool& strings,
                 ReportManager& reports,
                 SourceManager& source);
  ~CompileContext();

  bool compile(RefPtr<SourceFile> file);

  bool phasePassed() const {
    return !reports_.HasErrors();
  }
  bool canContinueProcessing() const {
    return !reports_.HasFatalError();
  }

  PoolAllocator& pool() {
    return pool_;
  }
  TypeManager* types() {
    return &types_;
  }
  SourceManager& source() {
    return source_;
  }
  CompileOptions& options() {
    return options_;
  }

  // String interning.
  Atom* add(const char* str) {
    return strings_.add(str);
  }
  Atom* add(const char* str, size_t length) {
    return strings_.add(str, length);
  }

  // Option changing.
  bool ChangePragmaDynamic(ReportingContext& rc, int64_t value);
  void SkipResolution() {
    options_.SkipResolution = true;
  }

  // Error reporting.
  ReportManager& reporting() {
    return reports_;
  }
  void reportFatal(rmsg::Id msg) {
    reports_.reportFatal(msg);
  }
  void reportFatal(const SourceLocation& loc, rmsg::Id msg) {
    reports_.reportFatal(loc, msg);
  }
  MessageBuilder report(const SourceLocation& loc, rmsg::Id msg_id) {
    return reports_.report(loc, msg_id);
  }
  MessageBuilder note(const SourceLocation& loc, rmsg::Id msg_id) {
    return reports_.note(loc, msg_id);
  }

  Atom* createAnonymousName(const SourceLocation& loc);

 private:
  PoolAllocator& pool_;
  StringPool& strings_;
  ReportManager& reports_;
  SourceManager& source_;
  TypeManager types_;
  CompileOptions options_;
};

extern ke::ThreadLocal<CompileContext*> CurrentCompileContext;

static inline PoolAllocator& POOL()
{
  return CurrentCompileContext->pool();
}

} // namespace ke

#endif // _include_jitcraft_compile_context_h_

