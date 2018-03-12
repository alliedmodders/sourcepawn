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

private:
  ast::Expression* node_;
  Type* type_;
};

class ConstValue : public Expr
{
public:
  explicit ConstValue(ast::Expression* node, Type* type, const BoxedValue& value)
   : Expr(node, type),
     value_(value)
  {}

private:
  BoxedValue value_;
};

} // namespace sema
} // namespace sp

#endif // _include_sourcepawn_sema_expressions_h_

