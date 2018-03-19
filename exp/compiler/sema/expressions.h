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

namespace ast {
class Expression;
} // namespace ast

namespace sema {

#define SEMA_KIND_LIST(_) \
  _(ConstValue)

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
  virtual ExprKind kind() const = 0;

#define _(name)                                 \
  virtual name##Expr* as##name##Expr() {        \
    return nullptr;                             \
  }                                             \
  virtual name##Expr* to##name##Expr() {        \
    assert(false);                              \
    return nullptr;                             \
  }                                             \
  virtual const char* prettyName() const = 0;
  SEMA_KIND_LIST(_)
#undef _

private:
  ast::Expression* node_;
  Type* type_;
};

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

class ConstValueExpr : public Expr
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

private:
  BoxedValue value_;
};

#undef DECLARE_SEMA
#undef SEMA_KIND_LIST

} // namespace sema
} // namespace sp

#endif // _include_sourcepawn_sema_expressions_h_

