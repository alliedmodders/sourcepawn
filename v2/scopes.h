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
#ifndef _include_jitcraft_scopes_h_
#define _include_jitcraft_scopes_h_

#include "pool-allocator.h"
#include "ast.h"
#include "scopes.h"

namespace ke {

class CompileContext;

typedef PoolList<Symbol *> SymbolList;

#define SCOPE_KIND_MAP(_)         \
  _(Global)                       \
  _(Function)                     \
  _(Block)

class GlobalScope;
class FunctionScope;
class BlockScope;

class Scope : public PoolObject
{
 protected:
  PoolAllocator &pool_;
  PoolList<Symbol *> names_;
  PoolList<Scope *> children_;
  
  bool empty() const {
    return names_.length() == 0;
  }
  void unlink();

 public:
  enum Kind {
#define _(name) name,
    SCOPE_KIND_MAP(_)
#undef _

    // Container scopes have no lexical meaning. They signal the name resolver
    // to look up one more scope.
    Proxy
  };

 public:
  Scope(PoolAllocator &pool, Kind kind, Scope *enclosing);
  bool initialize();

  Kind kind() const {
    return kind_;
  }
  Scope *enclosing() const {
    return enclosing_;
  }

  bool addSymbol(Symbol *symbol);

  Scope *unlinkIfEmpty();
  Symbol *localLookup(Atom *name);
  Symbol *lookup(Atom *name);

  void setParent(Scope *parent) {
    assert(!enclosing_ || enclosing_ == parent);
    enclosing_ = parent;
  }

  const PoolList<Symbol *> *symbols() const {
    return &names_;
  }

#define _(name)                                                               \
  bool is##name() {                                                           \
    return kind_ == name;                                                     \
  }                                                                           \
  name##Scope *as##name() {                                                   \
    if (is##name())                                                           \
      return (name##Scope *)this;                                             \
    return nullptr;                                                           \
  }                                                                           \
  name##Scope *to##name() {                                                   \
    assert(is##name());                                                       \
    return (name##Scope *)this;                                               \
  }
  SCOPE_KIND_MAP(_)
#undef _

 private:
  Kind kind_;
  Scope *enclosing_;
};

class FunctionScope : public Scope
{
 public:
  static FunctionScope *New(PoolAllocator &pool);

 private:
  FunctionScope(PoolAllocator &pool);
};

class LocalScope : public Scope
{
 public:
  static LocalScope *New(PoolAllocator &pool);

 private:
  LocalScope(PoolAllocator &pool);
};

class GlobalScope : public Scope
{
 public:
  static GlobalScope *New(PoolAllocator &pool);

  PoolList<Symbol *> *exported() {
    return &exported_;
  }
  void addPublic(Symbol *symbol) {
    exported_.append(symbol);
  }

 private:
  GlobalScope(PoolAllocator &pool);
  PoolList<Symbol *> exported_;
};

}

#endif // _include_jitcraft_scopes_h_
