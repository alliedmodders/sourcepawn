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
#include "am-float.h"
#include "constant-evaluator.h"
#include "compile-context.h"

using namespace ke;

static inline bool
ToBool(const BoxedPrimitive &box)
{
  if (box.isInt())
    return !!box.toInt();
  return box.toBool();
}

static inline void
CoerceToBool(BoxedPrimitive &in)
{
  if (in.isInt())
    in = BoxedPrimitive::Bool(!!in.toInt());
  else if (in.isFloat())
    in = BoxedPrimitive::Bool(!!in.toFloat());
  assert(in.isBool());
}

static inline bool
CoerceToFloat(BoxedPrimitive &in)
{
  assert(!in.isFloat());
  if (in.isInt()) {
    in = BoxedPrimitive::Float((float)in.toInt());
    return true;
  }
  return false;
}

static inline bool
CoerceForBinaryFloat(BoxedPrimitive &left, BoxedPrimitive &right)
{
  if (!(left.isFloat() || right.isFloat()))
    return false;
  if (!left.isFloat() && !CoerceToFloat(left))
    return false;
  if (!right.isFloat() && !CoerceToFloat(right))
    return false;
  return true;
}

// We need this since left % right is not defined in C++.
struct WrappedFloat {
  WrappedFloat(float f) : value(f)
  {}
  float value;
};
static inline float operator +(const WrappedFloat &left, const WrappedFloat &right) {
  return left.value + right.value;
}
static inline float operator -(const WrappedFloat &left, const WrappedFloat &right) {
  return left.value - right.value;
}
static inline float operator *(const WrappedFloat &left, const WrappedFloat &right) {
  return left.value * right.value;
}
static inline float operator /(const WrappedFloat &left, const WrappedFloat &right) {
  return left.value / right.value;
}
static inline float operator %(const WrappedFloat &left, const WrappedFloat &right) {
  return FloatModulo(left.value, right.value);
}

#define EVAL_ALU_OP(tok, op)                                                \
  case tok:                                                                 \
    if (CoerceForBinaryFloat(left, right)) {                                \
      WrappedFloat wleft(left.toFloat());                                   \
      WrappedFloat wright(right.toFloat());                                 \
      *out = BoxedPrimitive::Float(wleft op wright);                        \
      return Ok;                                                            \
    }                                                                       \
    if (left.isInt() && right.isInt()) {                                    \
      *out = BoxedPrimitive::Int(left.toInt() op right.toInt());            \
      return Ok;                                                            \
    }                                                                       \
    return TypeError;

#define EVAL_BIT_OP(tok, op)                                                \
  case tok:                                                                 \
    if (left.isInt() && right.isInt()) {                                    \
      *out = BoxedPrimitive::Int(left.toInt() op right.toInt());            \
      return Ok;                                                            \
    }                                                                       \
    return TypeError;

#define EVAL_LOGIC_OP(tok, op)                                              \
  case tok:                                                                 \
    if (!left.isBool())                                                     \
      CoerceToBool(left);                                                   \
    if (!right.isBool())                                                    \
      CoerceToBool(right);                                                  \
    *out = BoxedPrimitive::Bool(left.toBool() op right.toBool());           \
    return Ok;

#define EVAL_CMP_OP(tok, op)                                                \
  case tok:                                                                 \
    if (CoerceForBinaryFloat(left, right)) {                                \
      *out = BoxedPrimitive::Bool(left.toFloat() op right.toFloat());       \
      return Ok;                                                            \
    }                                                                       \
    if (left.isInt() && right.isInt()) {                                    \
      *out = BoxedPrimitive::Bool(left.toInt() op right.toInt());           \
      return Ok;                                                            \
    }                                                                       \
    return TypeError;

ConstantEvaluator::Result
ConstantEvaluator::binary(BinaryExpression *expr, BoxedPrimitive &left, BoxedPrimitive &right, BoxedPrimitive *out)
{
  switch (expr->token()) {
    EVAL_ALU_OP(TOK_PLUS, +);
    EVAL_ALU_OP(TOK_MINUS, -);
    EVAL_ALU_OP(TOK_STAR, *);
    EVAL_ALU_OP(TOK_SLASH, /);
    EVAL_ALU_OP(TOK_PERCENT, %);
    EVAL_BIT_OP(TOK_SHL, <<);
    EVAL_BIT_OP(TOK_SHR, >>);
    EVAL_BIT_OP(TOK_BITAND, &);
    EVAL_BIT_OP(TOK_BITOR, |);
    EVAL_LOGIC_OP(TOK_AND, &&);
    EVAL_LOGIC_OP(TOK_OR, ||);
    EVAL_CMP_OP(TOK_LT, <);
    EVAL_CMP_OP(TOK_LE, <=);
    EVAL_CMP_OP(TOK_GT, >); 
    EVAL_CMP_OP(TOK_GE, >=); 
    EVAL_CMP_OP(TOK_EQUALS, ==);
    EVAL_CMP_OP(TOK_NOTEQUALS, !=);

    default:
      return NotConstant;
  }
}

ConstantEvaluator::Result
ConstantEvaluator::unary(UnaryExpression *expr, const BoxedPrimitive &inner, BoxedPrimitive *out)
{
  switch (expr->token()) {
    case TOK_NEGATE:
      if (!inner.isInt())
        return TypeError;
      *out = BoxedPrimitive::Int(-inner.toInt());
      return TypeError;

    case TOK_TILDE:
      if (!inner.isInt())
        return TypeError;
      *out = BoxedPrimitive::Int(~inner.toInt());
      return Ok;

    case TOK_NOT:
      if (inner.isInt())
        *out = BoxedPrimitive::Bool(!inner.toInt());
      else if (inner.isFloat())
        *out = BoxedPrimitive::Bool(!inner.toFloat());
      else if (inner.isBool())
        *out = BoxedPrimitive::Bool(!inner.toBool());
      else
        return NotConstant;
      return Ok;

    default:
      return NotConstant;
  }
}

ConstantEvaluator::Result
ConstantEvaluator::Evaluate(Expression *expr, BoxedPrimitive *out)
{
  Result rv;
  if (BinaryExpression *b = expr->asBinaryExpression()) {
    BoxedPrimitive left;
    BoxedPrimitive right;
    if ((rv = Evaluate(b->left(), &left)) != Ok)
      return rv;
    if ((rv = Evaluate(b->right(), &right)) != Ok)
      return rv;
    rv = binary(b, left, right, out);
    if (rv == TypeError && mode_ == Required) {
      cc_.reportError(b->loc(), Message_InvalidBinaryType,
        TokenNames[b->token()],
        GetPrimitiveName(left.type()),
        GetPrimitiveName(right.type()));
    }
    return rv;
  }

  if (UnaryExpression *u = expr->asUnaryExpression()) {
    BoxedPrimitive inner;
    if ((rv = Evaluate(u->expression(), &inner)) != Ok)
      return rv;
    rv = unary(u, inner, out);
    if (rv == TypeError && mode_ == Required) {
      cc_.reportError(u->loc(), Message_InvalidUnaryType,
        TokenNames[u->token()],
        GetPrimitiveName(inner.type()));
    }
    return rv;
  }

  if (TernaryExpression *t = expr->asTernaryExpression()) {
    BoxedPrimitive cond;
    if ((rv = Evaluate(t->condition(), &cond)) != Ok)
      return rv;
    if (!cond.isBool())
      CoerceToBool(cond);
    Expression *result = cond.toBool() ? t->left() : t->right();
    return Evaluate(result, out);
  }

  if (IntegerLiteral *lit = expr->asIntegerLiteral()) {
    *out = BoxedPrimitive::Int((int32_t)lit->value());
    return Ok;
  }
  if (FloatLiteral *lit = expr->asFloatLiteral()) {
    *out = BoxedPrimitive::Float((float)lit->value());
    return Ok;
  }

  return Ok;
}
