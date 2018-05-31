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
#include "boxed-value.h"
#include "value-attrs.h"

namespace sp {

using namespace ke;

namespace ast {
class AstNode;
class FunctionStatement;
class VarDecl;
} // namespace ast

class String;

#define SYMBOL_KINDS(_) \
  _(Variable)           \
  _(Function)           \
  _(Constant)           \
  _(Type)               \
  _(Method)             \
  _(Field)              \
  _(Property)

#define _(name)   class name##Symbol;
SYMBOL_KINDS(_)
#undef _

class Scope;

// A symbol represents the declaration of a named entity.
class Symbol : public PoolObject
{
 public:
  enum Kind {
# define _(name) k##name,
    SYMBOL_KINDS(_)
# undef _
    kTotalSymbolKinds
  };

 public:
  Symbol(ast::AstNode* node, Scope* scope, Atom* name)
   : node_(node),
     scope_(scope),
     name_(name)
  {
  }

  virtual Kind kind() const = 0;
  virtual const char* kindName() const = 0;

  ast::AstNode* node() const {
    return node_;
  }
  Atom* name() const {
    return name_;
  }
  Scope* scope() const {
    return scope_;
  }

 public:
#define _(name)                                         \
  bool is##name() const {                               \
    return (kind() == k##name);                         \
  }                                                     \
  name##Symbol* as##name() {                            \
    if (is##name())                                     \
      return to##name();                                \
    return nullptr;                                     \
  }                                                     \
  name##Symbol* to##name() {                            \
    assert(is##name());                                 \
    return reinterpret_cast<name##Symbol*>(this);      \
  }
  SYMBOL_KINDS(_)
#undef _

 private:
  ast::AstNode* node_;
  Scope* scope_;
  Atom* name_;
};

class VariableSymbol : public Symbol
{
 public:
  VariableSymbol(ast::AstNode* node, Scope* scope, Atom* name)
   : Symbol(node, scope, name),
     storage_(StorageClass::Unknown),
     type_(nullptr)
  {
    address_ = 0;

    // Opaque == unresolved.
    constant_.setOpaqueIntptr(0);
  }

  Kind kind() const override {
    return kVariable;
  }
  const char* kindName() const override {
    return "variable";
  }

  void setType(Type* type) {
    assert(!type_ || type_->isUnresolvable());
    type_ = type;
  }
  void allocate(StorageClass storage, int32_t address) {
    storage_ = storage;
    address_ = address;
  }
  StorageClass storage() const {
    return storage_;
  }
  int32_t address() const {
    assert(storage() != StorageClass::Unknown);
    return address_;
  }
  Type* type() const {
    return type_;
  }

  // This can only be called after name binding.
  bool isArgument() const;

  // This can only be called after or during type resolution.
  bool canUseInConstExpr() const;

  // These are filled in during type resolution.
  bool isConstExpr() const {
    return !constant_.isOpaque();
  }
  const BoxedValue& constExpr() const {
    assert(isConstExpr());
    return constant_;
  }
  void setConstExpr(const BoxedValue& value) {
    // We're allowed to overwrite this if the original value was set to stop
    // infinite recursion, so no assert here.
    constant_ = value;
  }

  bool hasCheckedForConstExpr() const {
    return !isConstExpr() && !failedToResolveConstExpr();
  }

 private:
  static const int kResolvingConstExpr = 1;
  static const int kFailedResolveConstExpr = 2;

 public:
  void setResolvingConstExpr() {
    assert(!isConstExpr());
    constant_.setOpaqueIntptr(kResolvingConstExpr);
  }
  bool isResolvingConstExpr() const {
    return constant_.isOpaque() && constant_.toOpaqueIntptr() == kResolvingConstExpr;
  }

  // This state is currently unused - we may use it in the future if we want
  // to support non-constexpr const variables.
  void setFailedResolveConstExpr() {
    assert(isResolvingConstExpr());
    constant_.setOpaqueIntptr(kFailedResolveConstExpr);
  }
  bool failedToResolveConstExpr() const {
    return constant_.isOpaque() && constant_.toOpaqueIntptr() == kFailedResolveConstExpr;
  }

 private:
  StorageClass storage_;
  int32_t address_;
  Type* type_;
  BoxedValue constant_;
};

class TypeSymbol : public Symbol
{
 public:
  TypeSymbol(ast::AstNode* node, Scope* scope, Atom* name, Type* type = nullptr)
   : Symbol(node, scope, name),
     type_(type)
  {
  }

  Kind kind() const override {
    return kType;
  }
  const char* kindName() const override {
    return "type";
  }

  Type* type() const {
    return type_;
  }
  void setType(Type* type) {
    type_ = type;
  }

 private:
  Type* type_;
};

typedef PoolList<ast::FunctionStatement*> FuncStmtList;

class FunctionSymbol : public Symbol
{
 public:
  FunctionSymbol(ast::AstNode* node, Scope* scope, Atom* name)
   : Symbol(node, scope, name),
     shadows_(nullptr)
  {
  }

  Kind kind() const override {
    return kFunction;
  }
  const char* kindName() const override {
    return "function";
  }

  void addShadow(ast::FunctionStatement* stmt);
  FuncStmtList* shadows() const {
    return shadows_;
  }

  ast::FunctionStatement* impl() const;

 private:
  FuncStmtList* shadows_;
};

class ConstantSymbol : public Symbol
{
 public:
  ConstantSymbol(ast::AstNode* node, Scope* scope, Atom* name)
   : Symbol(node, scope, name)
  {
    // Opaque == unresolved.
    value_.setOpaqueIntptr(0);
  }
  ConstantSymbol(ast::AstNode* node, Scope* scope, Atom* name,
                 Type* type, const BoxedValue& value)
    : Symbol(node, scope, name),
      type_(type),
      value_(value)
  {
  }

  Kind kind() const override {
    return kConstant;
  }
  const char* kindName() const override {
    return "constant";
  }

  Type* type() const {
    return type_;
  }

  const BoxedValue& value() const {
    assert(hasValue());
    return value_;
  }
  bool hasValue() const {
    return !value_.isOpaque();
  }
  void setTypeAndValue(Type* type, const BoxedValue& value) {
    type_ = type;
    value_ = value;
  }

 private:
  static const intptr_t kResolving = 1;

 public:
  void setResolving() {
    assert(!hasValue() && !isResolving());
    value_.setOpaqueIntptr(kResolving);
  }
  bool isResolving() const {
    return value_.isOpaque() && value_.toOpaqueIntptr() == kResolving;
  }

 private:
  Type* type_;
  BoxedValue value_;
};

class FieldSymbol : public Symbol
{
 public:
  FieldSymbol(ast::AstNode* node, Scope* scope, Atom* name)
   : Symbol(node, scope, name),
     type_(nullptr)
  {
  }

  Kind kind() const override {
    return kField;
  }
  const char* kindName() const override {
    return "field";
  }

  void setType(Type* type) {
    assert(!type_);
    type_ = type;
  }
  Type* type() const {
    return type_;
  }

 private:
  Type* type_;
};

class PropertySymbol : public Symbol
{
 public:
  PropertySymbol(ast::AstNode* node, Scope* scope, Atom* name)
   : Symbol(node, scope, name),
     type_(nullptr)
  {
  }

  Kind kind() const override {
    return kProperty;
  }
  const char* kindName() const override {
    return "property";
  }

  void setType(Type* type) {
    assert(!type_);
    type_ = type;
  }
  Type* type() const {
    return type_;
  }

 private:
  Type* type_;
};

class MethodSymbol : public Symbol
{
 public:
  MethodSymbol(ast::AstNode* node, Scope* scope, Atom* name)
   : Symbol(node, scope, name)
  {
  }

  Kind kind() const override {
    return kMethod;
  }
  const char* kindName() const override {
    return "method";
  }
};

} // namespace sp

#endif // _include_sp2_symbol_h_
