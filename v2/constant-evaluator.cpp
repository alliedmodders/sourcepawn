/* vim: set ts=8 sts=2 sw=2 tw=99 et:
 *
 * Copyright (C) 2012-2014 David Anderson
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
#include "ast.h"
#include "constant-evaluator.h"

using namespace ke;

static inline bool
ToBool(const BoxedPrimitive &box)
{
  if (box.isInt())
    return !!box.toInt();
  return box.toBool();
}

#define EVAL_I_OP(type, tok, op)                                            \
  case tok:                                                                 \
    if (left.is##type() && right.is##type()) {                              \
      *out = BoxedPrimitive::type(left.to##type() op right.to##type());     \
      return true;                                                          \
    }                                                                       \
    return false;                                                           \

#define EVAL_L_OP(tok, op)                                                  \
  case tok:                                                                 \
    *out = BoxedPrimitive::Bool(ToBool(left) op ToBool(right));             \
    return true;

static inline bool
EvaluateBinaryOperation(TokenKind kind, const BoxedPrimitive &left, const BoxedPrimitive &right,
            BoxedPrimitive *out)
{
  switch (kind) {
   EVAL_I_OP(Int, TOK_PLUS, +);
   EVAL_I_OP(Int, TOK_MINUS, -);
   EVAL_I_OP(Int, TOK_STAR, *);
   EVAL_I_OP(Int, TOK_SLASH, /);
   EVAL_I_OP(Int, TOK_PERCENT, %);
   EVAL_I_OP(Int, TOK_SHL, <<);
   EVAL_I_OP(Int, TOK_SHR, >>);
   EVAL_I_OP(Int, TOK_BITAND, &);
   EVAL_I_OP(Int, TOK_BITOR, |);
   EVAL_L_OP(TOK_AND, &&);
   EVAL_L_OP(TOK_OR, ||);
   EVAL_L_OP(TOK_LT, <);
   EVAL_L_OP(TOK_LE, <=);
   EVAL_L_OP(TOK_GT, >); 
   EVAL_L_OP(TOK_GE, >=); 
   EVAL_L_OP(TOK_EQUALS, ==);
   EVAL_L_OP(TOK_NOTEQUALS, !=);

   default:
    return false;
  }
}

static inline bool
EvaluateUnaryOperation(TokenKind kind, const BoxedPrimitive &inner, BoxedPrimitive *out)
{
  switch (kind) {
   case TOK_NEGATE:
    if (inner.isInt()) {
      *out = BoxedPrimitive::Int(-inner.toInt());
      return true;
    }
    return false;

   case TOK_TILDE:
    if (inner.isInt()) {
      *out = BoxedPrimitive::Int(~inner.toInt());
      return true;
    }
    return false;

   case TOK_NOT:
    if (inner.isInt())
      *out = BoxedPrimitive::Bool(!inner.toInt());
    else
      *out = BoxedPrimitive::Bool(!inner.toBool());
    return true;

   default:
    return false;
  }
}

bool
ke::EvaluateForConstant(Expression *expr, BoxedPrimitive *out)
{
  if (BinaryExpression *b = expr->asBinaryExpression()) {
    BoxedPrimitive left;
    BoxedPrimitive right;
    if (!EvaluateForConstant(b->left(), &left) || !EvaluateForConstant(b->right(), &right))
      return false;
    return EvaluateBinaryOperation(b->token(), left, right, out);
  }

  if (UnaryExpression *u = expr->asUnaryExpression()) {
    BoxedPrimitive inner;
    if (!EvaluateForConstant(u->expression(), &inner))
      return false;
    return EvaluateUnaryOperation(u->token(), inner, out);
   }

  if (IntegerLiteral *lit = expr->asIntegerLiteral()) {
    *out = BoxedPrimitive::Int(lit->value());
    return true;
  }

  return true;
}
