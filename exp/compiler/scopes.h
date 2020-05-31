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
#include "scopes.h"

namespace sp {

class CompileContext;
class Symbol;

namespace ast {
class FieldDecl;
} // namespace ast

typedef PoolList<Symbol*> SymbolList;

#define SCOPE_KIND_MAP(_)         \
  _(Global)                       \
  _(Argument)                     \
  _(Block)                        \
  _(Layout)

// Global scope is the scope used for top-level user-defined names.
class GlobalScope;

// Argument scope is used to hold a function's name and arguments.
class ArgumentScope;

// Block scope is used for local variables inside a block or function body.
class BlockScope;

// Layout scope is used for symbols attached to a layout.
class LayoutScope;

class Scope : public PoolObject
{
 protected:
  PoolAllocator& pool_;
  PoolList<Symbol*> names_;
  PoolList<Scope*> children_;
  
  bool empty() const {
    return names_.size() == 0;
  }

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
  Scope(PoolAllocator& pool, Scope* enclosing);

  virtual Kind kind() const = 0;

  Scope* enclosing() const {
    return enclosing_;
  }

  void addSymbol(Symbol* symbol);

  Scope* unlinkIfEmpty();
  Symbol* localLookup(Atom* name);
  Symbol* lookup(Atom* name);

  void setParent(Scope* parent) {
    assert(!enclosing_ || enclosing_ == parent);
    enclosing_ = parent;
  }

  const PoolList<Symbol*>* symbols() const {
    return &names_;
  }

#define _(name)                                                               \
  bool is##name() {                                                           \
    return kind() == name;                                                    \
  }                                                                           \
  name##Scope* as##name() {                                                   \
    if (is##name())                                                           \
      return (name##Scope*)this;                                             \
    return nullptr;                                                           \
  }                                                                           \
  name##Scope* to##name() {                                                   \
    assert(is##name());                                                       \
    return (name##Scope*)this;                                               \
  }
  SCOPE_KIND_MAP(_)
#undef _

 private:
  Scope* enclosing_;
};

class ArgumentScope : public Scope
{
 public:
  static ArgumentScope* New(PoolAllocator& pool);

  virtual Kind kind() const override {
    return Argument;
  }

 private:
  ArgumentScope(PoolAllocator& pool);
};

class BlockScope : public Scope
{
 public:
  static BlockScope* New(PoolAllocator& pool);

  virtual Kind kind() const override {
    return Block;
  }

 private:
  BlockScope(PoolAllocator& pool);
};

class GlobalScope : public Scope
{
 public:
  static GlobalScope* New(PoolAllocator& pool);

  virtual Kind kind() const override {
    return Global;
  }

  PoolList<Symbol*>* exported() {
    return &exported_;
  }
  void addPublic(Symbol* symbol) {
    exported_.push_back(symbol);
  }

 private:
  GlobalScope(PoolAllocator& pool);
  PoolList<Symbol*> exported_;
};

class LayoutScope : public Scope
{
  LayoutScope(PoolAllocator& pool);

 public:
  static LayoutScope* New(PoolAllocator& pool);

  virtual Kind kind() const override {
    return Layout;
  }

  bool hasMixedAnonymousFields() const ;

  // Returns null if there are no anonymous fields.
  PoolList<ast::FieldDecl*>* anonymous_fields() const {
    return anonymous_fields_;
  }
  void addAnonymousField(ast::FieldDecl* decl);

 private:
  PoolList<ast::FieldDecl*>* anonymous_fields_;
};

}

#endif // _include_jitcraft_scopes_h_
