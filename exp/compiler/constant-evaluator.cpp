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
#include "parser/ast.h"
#include <amtl/am-float.h>
#include "constant-evaluator.h"
#include "compile-context.h"

namespace sp {

using namespace ke;
using namespace ast;

static inline void
CoerceToBool(BoxedValue& in)
{
  if (in.isInteger())
    in = BoxedValue(!in.toInteger().isZero());
  else if (in.isFloat())
    in = BoxedValue(!in.toFloat().isZero());
  assert(in.isBool());
}

static inline bool
CoerceToFloat(const FloatValue& ref, BoxedValue& in)
{
  assert(!in.isFloat());
  if (in.isInteger()) {
    if (ref.isDouble()) {
      if (in.toInteger().isSigned())
        in = BoxedValue(FloatValue::FromDouble((double)in.toInteger().asSigned()));
      else
        in = BoxedValue(FloatValue::FromDouble((double)in.toInteger().asUnsigned()));
    } else {
      if (in.toInteger().isSigned())
        in = BoxedValue(FloatValue::FromFloat((float)in.toInteger().asSigned()));
      else
        in = BoxedValue(FloatValue::FromFloat((float)in.toInteger().asUnsigned()));
    }
    return true;
  }
  return false;
}

static inline bool
CoerceForBinaryFloat(BoxedValue& left, BoxedValue& right)
{
  if (!(left.isFloat() || right.isFloat()))
    return false;
  if (!left.isFloat() && !CoerceToFloat(right.toFloat(), left))
    return false;
  if (!right.isFloat() && !CoerceToFloat(left.toFloat(), right))
    return false;
  return true;
}

#define EVAL_ALU_OP(tok, op)                                                \
  case tok:                                                                 \
    if (CoerceForBinaryFloat(left, right)) {                                \
      *out = BoxedValue(FloatValue::op(left.toFloat(), right.toFloat()));   \
      return Ok;                                                            \
    }                                                                       \
    if (left.isInteger() && right.isInteger()) {                            \
      IntValue tmp;                                                         \
      if (!IntValue::op(cc, left.toInteger(), right.toInteger(), &tmp))     \
        return TypeError;                                                   \
      *out = BoxedValue(tmp);                                               \
      return Ok;                                                            \
    }                                                                       \
    cc.report(rmsg::binary_type_mismatch)                                   \
      << TokenNames[tok]                                                    \
      << left.getTypename()                                                 \
      << right.getTypename();                                               \
    return TypeError;

#define EVAL_BIT_OP(tok, op)                                                \
  case tok:                                                                 \
    if (left.isInteger() && right.isInteger()) {                            \
      IntValue tmp;                                                         \
      if (!IntValue::op(cc, left.toInteger(), right.toInteger(), &tmp))     \
        return TypeError;                                                   \
      *out = BoxedValue(tmp);                                               \
      return Ok;                                                            \
    }                                                                       \
    cc.report(rmsg::binary_type_mismatch)                                   \
      << TokenNames[tok]                                                    \
      << left.getTypename()                                                 \
      << right.getTypename();                                               \
    return TypeError;

#define EVAL_LOGIC_OP(tok, op)                                              \
  case tok:                                                                 \
    if (!left.isBool())                                                     \
      CoerceToBool(left);                                                   \
    if (!right.isBool())                                                    \
      CoerceToBool(right);                                                  \
    *out = BoxedValue(left.toBool() op right.toBool());                     \
    return Ok;

#define EVAL_CMP_OP(tok, op)                                                \
  case tok:                                                                 \
    if (CoerceForBinaryFloat(left, right)) {                                \
      *out = BoxedValue(FloatValue::op(left.toFloat(), right.toFloat()));   \
      return Ok;                                                            \
    }                                                                       \
    if (left.isInteger() && right.isInteger()) {                            \
      bool tmp;                                                             \
      if (!IntValue::op(cc, left.toInteger(), right.toInteger(), &tmp))     \
        return TypeError;                                                   \
      *out = BoxedValue(tmp);                                               \
      return Ok;                                                            \
    }                                                                       \
    cc.report(rmsg::binary_type_mismatch)                                   \
      << TokenNames[tok]                                                    \
      << left.getTypename()                                                 \
      << right.getTypename();                                               \
    return TypeError;

ConstantEvaluator::Result
ConstantEvaluator::binary(BinaryExpression* expr, BoxedValue& left, BoxedValue& right, BoxedValue* out)
{
  ReportingContext cc(cc_, expr->loc(), mode_ == Required);

  switch (expr->token()) {
    EVAL_ALU_OP(TOK_PLUS, Add);
    EVAL_ALU_OP(TOK_MINUS, Sub);
    EVAL_ALU_OP(TOK_STAR, Mul);
    EVAL_ALU_OP(TOK_SLASH, Div);
    EVAL_ALU_OP(TOK_PERCENT, Mod);
    EVAL_BIT_OP(TOK_SHL, Shl);
    EVAL_BIT_OP(TOK_SHR, Shr); 
    EVAL_BIT_OP(TOK_USHR, Ushr); 
    EVAL_BIT_OP(TOK_BITAND, And);
    EVAL_BIT_OP(TOK_BITOR, Or);
    EVAL_BIT_OP(TOK_BITXOR, Xor);
    EVAL_LOGIC_OP(TOK_AND, &&);
    EVAL_LOGIC_OP(TOK_OR, ||);
    EVAL_CMP_OP(TOK_LT, Lt);
    EVAL_CMP_OP(TOK_LE, Le); 
    EVAL_CMP_OP(TOK_GT, Gt); 
    EVAL_CMP_OP(TOK_GE, Ge); 
    EVAL_CMP_OP(TOK_EQUALS, Eq);
    EVAL_CMP_OP(TOK_NOTEQUALS, Ne);

    default:
      return NotConstant;
  }
}

ConstantEvaluator::Result
ConstantEvaluator::unary(UnaryExpression* expr, BoxedValue& inner, BoxedValue* out)
{
  ReportingContext cc(cc_, expr->loc(), mode_ == Required);
  switch (expr->token()) {
    case TOK_NEGATE:
      if (inner.isInteger()) {
        IntValue tmp;
        if (!IntValue::Neg(cc, inner.toInteger(), &tmp))
          return TypeError;
        *out = BoxedValue(tmp);
        return Ok;
      }
      if (inner.isFloat()) {
        *out = BoxedValue(FloatValue::Neg(inner.toFloat()));
        return Ok;
      }
      cc.report(rmsg::unary_type_mismatch)
        << TokenNames[TOK_NEGATE]
        << inner.getTypename();
      return TypeError;

    case TOK_TILDE:
      if (!inner.isInteger()) {
        cc.report(rmsg::unary_type_mismatch)
          << TokenNames[TOK_TILDE]
          << inner.getTypename();
        return TypeError;
      }
      *out = BoxedValue(IntValue::Invert(inner.toInteger()));
      return Ok;

    case TOK_NOT:
      if (!inner.isBool())
        CoerceToBool(inner);
      *out = BoxedValue(!inner.toBool());
      return Ok;

    default:
      return NotConstant;
  }
}

ConstantEvaluator::Result
ConstantEvaluator::Evaluate(Expression* expr, BoxedValue* out)
{
  Result rv;
  if (BinaryExpression* b = expr->asBinaryExpression()) {
    BoxedValue left;
    BoxedValue right;
    if ((rv = Evaluate(b->left(), &left)) != Ok)
      return rv;
    if ((rv = Evaluate(b->right(), &right)) != Ok)
      return rv;
    return binary(b, left, right, out);
  }

  if (UnaryExpression* u = expr->asUnaryExpression()) {
    BoxedValue inner;
    if ((rv = Evaluate(u->expression(), &inner)) != Ok)
      return rv;
    return unary(u, inner, out);
  }

  if (TernaryExpression* t = expr->asTernaryExpression()) {
    BoxedValue cond;
    if ((rv = Evaluate(t->condition(), &cond)) != Ok)
      return rv;
    if (!cond.isBool())
      CoerceToBool(cond);
    Expression* result = cond.toBool() ? t->left() : t->right();
    return Evaluate(result, out);
  }

  if (IntegerLiteral* lit = expr->asIntegerLiteral()) {
    if (lit->value() >= INT_MIN && lit->value() <= INT_MAX)
      *out = BoxedValue(IntValue::FromInt32((int32_t)lit->value()));
    else
      *out = BoxedValue(IntValue::FromInt64(lit->value()));
    return Ok;
  }
  if (FloatLiteral* lit = expr->asFloatLiteral()) {
    *out = BoxedValue(FloatValue::FromFloat((float)lit->value()));
    return Ok;
  }
  if (NameProxy* proxy = expr->asNameProxy()) {
    if (ConstantSymbol* cs = proxy->sym()->asConstant()) {
      *out = resolver_->resolveValueOfConstant(cs);
      return Ok;
    }
    if (VariableSymbol* sym = proxy->sym()->asVariable()) {
      if (resolver_->resolveVarAsConstant(sym, out))
        return Ok;
      return NotConstant;
    }
    return NotConstant;
  }
  if (SizeofExpression* so = expr->asSizeofExpression()) {
    ReportingContext cc(cc_, so->loc(), mode_ == Required);

    // Resolve to a variable - not a var or type like C++.
    VariableSymbol* sym = so->proxy()->sym()->asVariable();
    if (!sym) {
      cc.report(rmsg::sizeof_needs_variable);
      return TypeError;
    }

    Type* type = resolver_->resolveTypeOfVar(sym);
    int32_t value = ComputeSizeOfType(cc, type, so->level());
    if (!value)
      return TypeError;

    *out = BoxedValue(IntValue::FromInt32(value));
    return Ok;
  }

  return NotConstant;
}

} // namespace sp
