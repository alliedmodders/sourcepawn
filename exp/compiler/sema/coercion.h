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
#ifndef _include_sema_eval_context_h_
#define _include_sema_eval_context_h_

#include "parser/ast.h"
#include "sema/expressions.h"

namespace sp {

enum class CoercionKind
{
  Invalid,

  // A boolean test for a conditional or logical operation.
  Test,

  // Catch-all case for coercions within expressions.
  Expr,

  // Stop at the lvalue-to-rvalue step.
  RValue,

  // This coercion is for an argument.
  Arg,

  // This coercion is for the index of an array index expression.
  Index,

  // This coercion is for assignment.
  Assignment,

  // Expression in a return statement.
  Return
};

struct EvalContext
{
  CoercionKind ck = CoercionKind::Invalid;
  ast::Expression* from_src = nullptr;
  sema::Expr* from = nullptr;
  Type* to = nullptr;
  sema::Expr* result = nullptr;

  EvalContext()
  {}
  EvalContext(CoercionKind ck,
              sema::Expr* from,
              Type* to)
   : ck(ck),
     from_src(from->src()),
     from(from),
     to(to)
  {}
  EvalContext(CoercionKind ck,
              ast::Expression* from_src,
              Type* to)
   : ck(ck),
     from_src(from_src),
     from(nullptr),
     to(to)
  {}
};

struct TestEvalContext : EvalContext
{
  TestEvalContext(CompileContext& cc, sema::Expr* from);
  TestEvalContext(CompileContext& cc, ast::Expression* from);
};

struct LValueToRValueContext : EvalContext
{
  explicit LValueToRValueContext(ast::Expression* from);
  explicit LValueToRValueContext(sema::Expr* from);
};

struct TernaryContext
{
  TernaryContext(ast::Expression* left, ast::Expression* right)
   : left_src(left),
     right_src(right),
     left(nullptr),
     right(nullptr),
     type(nullptr)
  {}

  ast::Expression* left_src;
  ast::Expression* right_src;
  sema::Expr* left;
  sema::Expr* right;
  Type* type;
};

} // namespace sp

#endif // _include_sema_eval_context_h_

