// vim: set ts=8 sw=2 sts=2 tw=99 et:
// 
// Copyright (C) 2012-2014 David Anderson, AlliedModders LLC
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

#ifndef _include_sp2_symbol_h_
#define _include_sp2_symbol_h_

#include "pool-allocator.h"
#include "types.h"
#include "label.h"

namespace ke {

class String;
class AstNode;

#define SYMBOL_KINDS(_) \
  /* Any kind of variable or argument produces a VariableSymbol. */ \
  _(Variable)     \
  /* A function declaration produces a FunctionSymbol. */ \
  _(Function)     \
  /* A named constant produces a ConstantSymbol. */ \
  _(Constant)     \
  /* A named type (class struct, typedef, etc) produces a TypeSymbol. */ \
  _(Type)       \
  /* An import name. */ \
  _(Import)       \
  /* A name implicitly derived from an import. */ \
  _(Implicit)

#define _(name)   class name##Symbol;
SYMBOL_KINDS(_)
#undef _

class Scope;

// A symbol represents the declaration of a named entity.
class Symbol : public PoolObject
{
 public:
  enum Kind {
#     define _(name) k##name,
    SYMBOL_KINDS(_)
#     undef _
    kTotalSymbolKinds
  };

 public:
  Symbol(AstNode *node, Scope *scope, Atom *name)
   : node_(node),
     scope_(scope),
     name_(name)
  {
  }

  virtual Kind kind() const = 0;

  AstNode *node() const {
    return node_;
  }
  Atom *name() const {
    return name_;
  }
  Scope *scope() const {
    return scope_;
  }

 public:
#define _(name)                                         \
  bool is##name() const {                               \
    return (kind() == k##name);                         \
  }                                                     \
  name##Symbol *as##name() {                            \
    if (is##name())                                     \
      return to##name();                                \
    return nullptr;                                     \
  }                                                     \
  name##Symbol *to##name() {                            \
    assert(is##name());                                 \
    return reinterpret_cast<name##Symbol *>(this);      \
  }
  SYMBOL_KINDS(_)
#undef _

 private:
  AstNode *node_;
  Scope *scope_;
  Atom *name_;
};

class VariableSymbol : public Symbol
{
 public:
  enum Storage {
    Unknown,
    Local,
    Arg,
    Heap,
    Global
  };

 public:
  VariableSymbol(AstNode *node, Scope *scope, Atom *name)
   : Symbol(node, scope, name),
     storage_(Unknown)
  {
  }

  VariableSymbol(AstNode *node, Scope *scope, Atom *name, Type *type)
   : Symbol(node, scope, name),
     storage_(Unknown),
     type_(type)
  {
  }

  Kind kind() const {
    return kVariable;
  }
  void setType(Type *type) {
    assert(!type_);
    type_ = type;
  }
  void allocate(Storage storage, intptr_t address) {
    storage_ = storage;
    address_ = address;
  }
  Storage storage() const {
    return storage_;
  }
  intptr_t address() const {
    assert(storage() != Unknown);
    return address_;
  }
  Type *type() const {
    return type_;
  }

 private:
  Storage storage_;
  intptr_t address_;
  Type *type_;
};

class TypeSymbol : public Symbol
{
 public:
  TypeSymbol(AstNode *node, Scope *scope, Atom *name, Type *type)
   : Symbol(node, scope, name),
     type_(type)
  {
  }

  Kind kind() const {
    return kType;
  }

  Type *type() const {
    return type_;
  }
  void setType(Type *type) {
    type_ = type;
  }

 private:
  Type *type_;
};

typedef PoolList<Symbol *> SymbolList;

class FunctionSymbol : public Symbol
{
 public:
  FunctionSymbol(AstNode *node, Scope *scope, Atom *name)
   : Symbol(node, scope, name),
     shadows_(nullptr)
  {
  }

  Kind kind() const {
    return kFunction;
  }
  Label *address() {
    return &address_;
  }

  void setShadows(SymbolList *shadows) {
    shadows_ = shadows;
  }
  SymbolList *shadows() const {
    return shadows_;
  }

 private:
  Label address_;
  SymbolList *shadows_;
};

class ConstantSymbol : public Symbol
{
 public:
  ConstantSymbol(AstNode *node, Scope *scope, Atom *name, Type *type)
   : Symbol(node, scope, name),
     type_(type)
  {
  }
  ConstantSymbol(AstNode *node, Scope *scope, Atom *name, Type *type,
                 const BoxedPrimitive &prim)
    : Symbol(node, scope, name),
      type_(type),
      value_(prim)
  {
    type_ = type;
  }

  Kind kind() const {
    return kConstant;
  }

  const BoxedPrimitive &value() const {
    return value_;
  }
  void setValue(const BoxedPrimitive &value) {
    value_ = value;
  }
  Type *type() const {
    return type_;
  }

 private:
  Type *type_;
  BoxedPrimitive value_;
};

}

#endif // _include_sp2_symbol_h_
