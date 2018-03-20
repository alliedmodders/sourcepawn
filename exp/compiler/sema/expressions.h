// vim: set ts=2 sw=2 tw=99 et:
// 
// Copyright (C) 2012-2014 AlliedModders LLC and David Anderson
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
#ifndef _include_sourcepawn_sema_expressions_h_
#define _include_sourcepawn_sema_expressions_h_

#include "boxed-value.h"
#include "pool-allocator.h"

namespace sp {

class Type;
class FunctionSymbol;
class VariableSymbol;

namespace ast {
class Expression;
} // namespace ast

namespace sema {

#define SEMA_KIND_LIST(_) \
  _(Binary)               \
  _(Unary)                \
  _(Call)                 \
  _(ConstValue)           \
  _(NamedFunction)        \
  _(Var)                  \
  _(TrivialCast)          \
  _(String)               \
  _(IncDec)               \
  _(StructInit)           \
  _(Index)                \
  /* terminator */

// Forward declarations.
#define _(name) class name##Expr;
SEMA_KIND_LIST(_)
#undef _

enum class ExprKind
{
#define _(name) name,
  SEMA_KIND_LIST(_)
#undef _
  Sentinel
};

class Expr : public PoolObject
{
public:
  explicit Expr(ast::Expression* node, Type* type)
    : node_(node),
      type_(type)
  {}

  Type* type() const {
    return type_;
  }
  ast::Expression* src() const {
    return node_;
  }
  virtual ExprKind kind() const = 0;
  virtual const char* prettyName() const = 0;

  virtual bool isConstant() const {
    return false;
  }
  virtual bool getBoxedValue(BoxedValue* out) const {
    return false;
  }
  virtual bool getConstantInt32(int32_t* value) const {
    return false;
  }

#define _(name)                                 \
  virtual name##Expr* as##name##Expr() {        \
    return nullptr;                             \
  }                                             \
  virtual name##Expr* to##name##Expr() {        \
    assert(false);                              \
    return nullptr;                             \
  }
  SEMA_KIND_LIST(_)
#undef _

private:
  ast::Expression* node_;
  Type* type_;
};

typedef PoolList<Expr*> ExprList;

#define DECLARE_SEMA(name)                  \
  ExprKind kind() const override {          \
    return ExprKind::name;                  \
  }                                         \
  name##Expr* as##name##Expr() override {   \
    return this;                            \
  }                                         \
  name##Expr* to##name##Expr() override {   \
    return this;                            \
  }                                         \
  const char* prettyName() const override { \
    return #name "Expr";                    \
  }

class ConstValueExpr final : public Expr
{
public:
  explicit ConstValueExpr(ast::Expression* node, Type* type, const BoxedValue& value)
   : Expr(node, type),
     value_(value)
  {}

  DECLARE_SEMA(ConstValue)

  const BoxedValue& value() const {
    return value_;
  }
  bool isConstant() const override {
    return true;
  }
  bool getBoxedValue(BoxedValue* out) const override {
    *out = value_;
    return true;
  }
  bool getConstantInt32(int32_t* value) const override {
    if (!value_.isInteger())
      return false;
    if (!value_.toInteger().valueFitsInInt32())
      return false;
    *value = value_.toInteger().asSigned();
    return true;
  }

private:
  BoxedValue value_;
};

class BinaryExpr final : public Expr
{
 public:
  explicit BinaryExpr(ast::Expression* node,
                      Type* type,
                      TokenKind tok,
                      Expr* left,
                      Expr* right)
   : Expr(node, type),
     token_(tok),
     left_(left),
     right_(right)
  {}

  DECLARE_SEMA(Binary)

  TokenKind token() const {
    return token_;
  }
  Expr* left() const {
    return left_;
  }
  Expr* right() const {
    return right_;
  }

 private:
  TokenKind token_;
  Expr* left_;
  Expr* right_;
};

class UnaryExpr final : public Expr
{
 public:
  explicit UnaryExpr(ast::Expression* node,
                     Type* type,
                     TokenKind tok,
                     Expr* expr)
   : Expr(node, type),
     token_(tok),
     expr_(expr)
  {}

  DECLARE_SEMA(Unary)

  TokenKind token() const {
    return token_;
  }
  Expr* expr() const {
    return expr_;
  }

 private:
  TokenKind token_;
  Expr* expr_;
};

class IncDecExpr final : public Expr
{
 public:
  explicit IncDecExpr(ast::Expression* node,
                     Type* type,
                     TokenKind tok,
                     Expr* expr,
                     bool postfix)
   : Expr(node, type),
     token_(tok),
     expr_(expr),
     postfix_(postfix)
  {}

  DECLARE_SEMA(IncDec)

  TokenKind token() const {
    return token_;
  }
  Expr* expr() const {
    return expr_;
  }
  bool prefix() const {
    return !postfix_;
  }
  bool postfix() const {
    return postfix_;
  }

 private:
  TokenKind token_;
  Expr* expr_;
  bool postfix_;
};

class TrivialCastExpr final : public Expr
{
 public:
  explicit TrivialCastExpr(ast::Expression* node,
                           Type* type,
                           Expr* expr)
   : Expr(node, type),
     expr_(expr)
  {}

  DECLARE_SEMA(TrivialCast)

  Expr* expr() const {
    return expr_;
  }

 private:
  Expr* expr_;
};

class NamedFunctionExpr final : public Expr
{
 public:
  explicit NamedFunctionExpr(ast::Expression* node,
                             Type* type,
                             FunctionSymbol* sym)
   : Expr(node, type),
     sym_(sym)
  {}

  DECLARE_SEMA(NamedFunction)

  FunctionSymbol* sym() const {
    return sym_;
  }

 private:
  FunctionSymbol* sym_;
};

class CallExpr final : public Expr
{
 public:
  explicit CallExpr(ast::Expression* node,
                    Type* type,
                    Expr* callee,
                    ExprList* args)
   : Expr(node, type),
     callee_(callee),
     args_(args)
  {}

  DECLARE_SEMA(Call)

  Expr* callee() const {
    return callee_;
  }
  ExprList* args() const {
    return args_;
  }

 private:
  Expr* callee_;
  ExprList* args_;
};

class VarExpr final : public Expr
{
 public:
  explicit VarExpr(ast::Expression* node,
                   Type* type,
                   VariableSymbol* sym)
   : Expr(node, type),
     sym_(sym)
  {}

  DECLARE_SEMA(Var)

  VariableSymbol* sym() const {
    return sym_;
  }

 private:
  VariableSymbol* sym_;
};

class StringExpr final : public Expr
{
 public:
  explicit StringExpr(ast::Expression* node,
                      Type* type,
                      Atom* literal)
   : Expr(node, type),
     literal_(literal)
  {}

  DECLARE_SEMA(String)

  Atom* literal() const {
    return literal_;
  }

 private:
  Atom* literal_;
};

class StructInitExpr final : public Expr
{
 public:
  explicit StructInitExpr(ast::Expression* node,
                          Type* type,
                          ExprList* exprs)
   : Expr(node, type),
     exprs_(exprs)
  {}

  DECLARE_SEMA(StructInit)

  bool isConstant() const override {
    return true;
  }
  ExprList* exprs() const {
    return exprs_;
  }

 private:
  ExprList* exprs_;
};

class IndexExpr final : public Expr
{
 public:
  explicit IndexExpr(ast::Expression* node,
                      Type* type,
                      Expr* base,
                      Expr* index)
   : Expr(node, type),
     base_(base),
     index_(index)
  {}

  DECLARE_SEMA(Index)

  Expr* base() const {
    return base_;
  }
  Expr* index() const {
    return index_;
  }

 private:
  Expr* base_;
  Expr* index_;
};

#undef DECLARE_SEMA
#undef SEMA_KIND_LIST

} // namespace sema
} // namespace sp

#endif // _include_sourcepawn_sema_expressions_h_

