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

#include "compile-context.h"
#include "semantic-analysis.h"
#include "coercion.h"

namespace sp {

using namespace ke;
using namespace ast;

bool
SemanticAnalysis::coerce(EvalContext& ec)
{
  // First evaluate from_src if needed. This is just a helper step to reduce
  // boilerplate in callers.
  if (!ec.from) {
    assert(ec.from_src);
    ec.from = visitExpression(ec.from_src);
    if (!ec.from)
      return false;
    if (!ec.to) {
      assert(ec.ck == CoercionKind::RValue);
      ec.to = ec.from->type();
    }
  }

  // Our initial result starts from the given expression.
  ec.result = ec.from;

  Type* from = ec.result->type();
  Type* to = ec.to;

  // If types have the same identity, we can shortcut and return immediately,
  // unless the coercion kind is RValue.
  if (from == to) {
    if (sema::LValueExpr* lvalue = ec.result->asLValueExpr())
      ec.result = lvalue_to_rvalue(lvalue);
    return true;
  }

  // These checks occur before the lvalue-to-rvalue step, since whether or not
  // we have an lvalue can change which instructions we emit.
  if (to->isArray())
    return coerce_array(ec);
  if (to->isReference())
    return coerce_ref(ec);

  // If we get here, we're coercing to a non-addressable type, so we should
  // force a load of any l-values.
  if (sema::LValueExpr* lvalue = ec.result->asLValueExpr()) {
    ec.result = lvalue_to_rvalue(lvalue);
    from = ec.result->type();
  }

  switch (to->canonicalKind()) {
    case Type::Kind::Primitive:
      return coerce_primitive(ec);

    case Type::Kind::Void:
      if (from->isVoid())
        return true;
      return no_conversion(ec);
  }

  return no_conversion(ec);
}

bool
SemanticAnalysis::coerce_array(EvalContext& ec)
{
  ArrayType* to_array = ec.to->toArray();

  // As a special case, a[n] coerces to type(a)[], an ancient feature that may
  // or may not have originally been intentional. This will be non-null if we
  // have to create a slice.
  sema::IndexExpr* index_expr;
  Type* from = arrayOrSliceType(ec, &index_expr);
  if (!from)
    return no_conversion(ec);

  // Arrays should not appear in the |to| field of most coercion kinds, yet.
  assert(ec.ck == CoercionKind::RValue ||
         ec.ck == CoercionKind::Assignment ||
         ec.ck == CoercionKind::Arg);

  // Assignment/return operators should never let multi-dimensional arrays enter here.
  // :TODO: ensure
  if (ec.ck == CoercionKind::Assignment || ec.ck == CoercionKind::Return)
    assert(to_array->nlevels() == 1);

  // Check that each level contains a matching size and const-qualifier.
  Type* from_iter = from;
  Type* to_iter = to_array;
  while (from_iter->isArray() && to_iter->isArray()) {
    ArrayType* from_iter_array = from_iter->toArray();
    ArrayType* to_iter_array = to_iter->toArray();

    // For passing to an argument, a fixed-length array can coerce to a non-
    // fixed length array. I.e. int[2][3][4] should coerce to int[][][]. But,
    // if the destination has a fixed size, it must always match.
    if (to_iter_array->hasFixedLength()) {
      if (!from_iter_array->hasFixedLength())
        return no_conversion(ec);

      if (ec.ck == CoercionKind::Assignment) {
        // As a special exception, we allow converting from char[N] to char[M]
        // if N<=M, since strings are null-terminated.
        if (from_iter_array->fixedLength() > to_iter_array->fixedLength())
          return no_conversion(ec);
      } else {
        if (from_iter_array->fixedLength() != to_iter_array->fixedLength())
          return no_conversion(ec);
      }
    }

    from_iter = from_iter_array->contained();
    to_iter = to_iter_array->contained();

    // We disallow coercions when it would implicitly allow an illegal
    // assignment. For example:
    //   int x[10][20];
    //
    //   void function(int p[][]) {
    //     p[1] = {...};
    //   }
    //
    // Currently, this assignment is a type error. In the future it might not
    // be, and want to leave open the possibility of garbage-collected,
    // pointer-escaping arrays. In such a model, the assignment above must
    // *still* be a type error , otherwise x[1] no longer points to an int[20].
    //
    // The only way to avoid making this a type error would be to change *all*
    // array semantics, and remove the concept of fixed-length arrays. We are
    // not yet prepared to go there.
    //
    // Note: from_iter and to_iter are now the contained variants, and
    // |from_iter_array| and |to_iter_array| are the enclosing type.
    if (from_iter->isArray() &&
        to_iter->isArray() &&
        from_iter->toArray()->hasFixedLength() &&
        !to_iter->toArray()->hasFixedLength() &&
        !to_iter->isConst())
    {
      cc_.report(ec.from_src->loc(), rmsg::coercion_allows_illegal_assn) <<
        ec.from->type() << ec.to;
      ec.result = nullptr;
      return false;
    }

    if (ec.ck == CoercionKind::Arg) {
      // If the source contents at this level are const, but the target wants
      // something mutable, then no conversion is available.
      if (from_iter->isConst() && !to_iter->isConst())
        return no_conversion(ec);
    } else if (ec.ck == CoercionKind::Assignment) {
      // const int p[10] is not assignable. :TODO: handle this in assignment
      // const int p[] has no semantics at all. :TODO: block this in assignment
      assert(to_iter_array->hasFixedLength());
      assert(!to_iter->isConst());
    }
  }

  // The innermost types must be identical.
  from_iter = from_iter->unqualified();
  to_iter = to_iter->unqualified();
  if (!CompareNonArrayTypesExactly(from_iter, to_iter))
    return no_conversion(ec);

  // Phew... everything is equal. If the destination type is fixed-length,
  // there is nothing more to do.
  if (to_array->hasFixedLength()) {
    assert(!index_expr);
    assert(from->toArray()->hasFixedLength());
    return true;
  }

  // Create a slice if needed.
  if (index_expr) {
    sema::Expr* base = index_expr->base();
    if (sema::LValueExpr* lval = base->asLValueExpr())
      base = new (pool_) sema::LoadExpr(base->src(), base->type(), lval);
    ec.result = new (pool_) sema::SliceExpr(base->src(), from, base, index_expr->index());
    return true;
  }

  if (sema::LValueExpr* lval = ec.result->asLValueExpr())
    ec.result = lvalue_to_rvalue(lval);
  return true;
}

Type*
SemanticAnalysis::arrayOrSliceType(EvalContext& ec, sema::IndexExpr** out)
{
  Type* from = ec.result->type();
  ArrayType* to = ec.to->toArray();

  // Ideal case: expression ranks match as-is.
  if (from->isArray() &&
      from->toArray()->nlevels() == to->nlevels())
  {
    *out = nullptr;
    return from;
  }

  // Only arguments can create implicit slices.
  if (ec.ck != CoercionKind::Arg)
    return nullptr;

  // If the destination is fixed-length, then slices won't work.
  if (to->hasFixedLength())
    return nullptr;

  sema::IndexExpr* index_expr = ec.result->asIndexExpr();
  if (!index_expr)
    return nullptr;

  from = index_expr->base()->type();
  if (from->toArray()->hasFixedLength()) {
    // Implicit slices cannot create fixed-length arrays, so we need to rewrite
    // the outgoing type.
    from = types_->newArray(from->toArray()->contained(), ArrayType::kUnsized);
  }

  *out = index_expr;
  return from;
}

bool
SemanticAnalysis::coerce_ref(EvalContext& ec)
{
  ReferenceType* to = ec.to->asReference();

  Type* from = ec.result->type();
  if (from->isReference())
    from = from->toReference()->inner();

  assert(!to->inner()->isArray());

  // We will probably have to relax this for retagging.
  bool equal = CompareNonArrayTypesExactly(
    to->inner()->unqualified(),
    from->unqualified());
  if (!equal)
    return no_conversion(ec);

  if (ec.ck == CoercionKind::Arg) {
    if (from->isConst() && !to->inner()->isConst())
      return no_conversion(ec);

    // Only an l-value can flow into an argument reference.
    if (!ec.result->asLValueExpr()) {
      cc_.report(ec.result->src()->loc(), rmsg::illegal_lvalue);
      return false;
    }
    return true;
  }

  // :TODO: figure out when these cases come up.
  return no_conversion(ec);
}

sema::Expr*
SemanticAnalysis::coerce_arg(ast::Expression* ast_expr, Type* to)
{
  sema::Expr* expr = visitExpression(ast_expr);
  if (!expr)
    return nullptr;

  if (VariadicType* vararg = to->asVariadic()) {
    // Peel away to the inner type.
    to = vararg->inner();

    // If the inner type is "unchecked", we do not want any sort of coercion or
    // even an lvalue-to-rvalue coercion unless it's for a pointer type.
    Type* from = expr->type();
    if (to->isUnchecked()) {
      if (expr->isLValueExpr() && TypeHasIndirectLValue(from))
        return lvalue_to_rvalue(expr->asLValueExpr());
      return expr;
    }

    // The goal of coerce_vararg is to preserve addresses if possible. If that's
    // not an issue (for example, the type is an r-value or an array), then we
    // can use the normal coercion logic. If there is an l-value we should
    // preserve, we have special logic for that.
    if (expr->isLValueExpr() && !to->isArray())
      return coerce_vararg(expr->asLValueExpr(), to);
  }

  EvalContext ec(CoercionKind::Arg, expr, to);
  if (!coerce(ec))
    return nullptr;
  return ec.result;
}

sema::Expr*
SemanticAnalysis::coerce_vararg(sema::LValueExpr* expr, Type* to)
{
  Type* from = expr->storedType();
  if (CompareNonArrayTypesExactly(from->unqualified(), to->unqualified()) &&
      (!from->isConst() || to->isConst()))
  {
    // No conversion is needed, we can keep the l-value.
    assert(!TypeHasIndirectLValue(from));
    return expr;
  }

  // Otherwise, we have a const -> non-const coercion or something else. We
  // can't use the l-value.
  EvalContext ec(CoercionKind::Arg, expr, to);
  if (!coerce(ec))
    return nullptr;

  cc_.report(expr->src()->loc(), rmsg::vararg_losing_lvalue) <<
    from << to;
  return ec.result;
}

bool
SemanticAnalysis::coerce_primitive(EvalContext& ec)
{
  Type* from = ec.result->type();
  Type* to = ec.to;

  if (!from->isPrimitive())
    return no_conversion(ec);

  if (from->primitive() == to->primitive())
    return true;

  switch (to->primitive()) {
    case PrimitiveType::Int32:
      if (from->primitive() == PrimitiveType::Char ||
          (from->primitive() == PrimitiveType::Bool && ec.ck == CoercionKind::Arg))
      {
        // :TODO: char should become a ZeroExtend operation.
        ec.result = new (pool_) sema::ImplicitCastExpr(
          ec.from_src,
          to,
          sema::CastOp::None,
          ec.result);
        return true;
      }
      return no_conversion(ec);

    case PrimitiveType::Bool:
      ec.result = new (pool_) sema::ImplicitCastExpr(
        ec.from_src,
        to,
        sema::CastOp::None,
        ec.result);
      return true;
    case PrimitiveType::Char:
      return coerce_to_char(ec);
  }
  return no_conversion(ec);
}

bool
SemanticAnalysis::coerce_to_char(EvalContext& ec)
{
  Type* from = ec.result->type();
  Type* to = ec.to;

  if (from->primitive() != PrimitiveType::Int32)
    return no_conversion(ec);

  assert(ec.ck == CoercionKind::Arg ||
         ec.ck == CoercionKind::Assignment ||
         ec.ck == CoercionKind::Return);

  // We allow a truncating conversion from int to char. For literals, we warn
  // first if the value is too big.
  int32_t value;
  if (ec.result->getConstantInt32(&value)) {
    // Note that we allow the full signed and unsigned range.
    if (value >= SCHAR_MIN && value <= UCHAR_MAX) {
      IntValue iv = (value > SCHAR_MAX)
                    ? IntValue::FromUnsigned(value, 8)
                    : IntValue::FromSigned(value, 8);
      ec.result = new (pool_) sema::ConstValueExpr(
        ec.result->src(),
        to,
        BoxedValue(iv));
      return true;
    }

    cc_.report(ec.result->src()->loc(), rmsg::constant_will_truncate)
      << to;
  }

  // No implicit truncation needed.
  ec.result = new (pool_) sema::ImplicitCastExpr(
    ec.result->src(),
    ec.to,
    sema::CastOp::TruncateInt,
    ec.result);
  return true;
}

// :TODO: test
bool
SemanticAnalysis::coerce_ternary(TernaryContext& tc)
{
  // Convert both the left and right-hand sides to rvalues.
  {
    LValueToRValueContext ec(tc.left_src);
    if (!coerce(ec))
      return false;
    tc.left = ec.result;
  }
  {
    LValueToRValueContext ec(tc.right_src);
    if (!coerce(ec))
      return false;
    tc.right = ec.result;
  }

  ArrayType* left_array = tc.left->type()->asArray();
  ArrayType* right_array = tc.right->type()->asArray();
  if (left_array && right_array) {
    bool match = false;
    bool exact_match = true;
    Type* innermost = nullptr;
    size_t nlevels = 0;
    while (left_array && right_array) {
      if ((left_array->hasFixedLength() != right_array->hasFixedLength()) ||
          (left_array->hasFixedLength() && right_array->hasFixedLength() &&
           left_array->fixedLength() != right_array->fixedLength()))
      {
        exact_match = false;
      }

      nlevels++;

      Type* left = left_array->contained();
      Type* right = right_array->contained();

      if (left->qualifiers() != right->qualifiers())
        exact_match = false;

      left_array = left->asArray();
      right_array = right->asArray();

      // Neither is an array, we've hit the innermost type.
      if (!left_array && !right_array) {
        match = CompareNonArrayTypesExactly(
          left->unqualified(),
          right->unqualified());
        innermost = left;
        break;
      }

      // One array type goes deeper than the other; mismatch.
      if (left_array || right_array) {
        match = false;
        break;
      }
    }

    // Types have no common conversion.
    if (!match) {
      cc_.report(tc.left_src->loc(), rmsg::no_common_conversion)
        << tc.left->type() << tc.right->type();
      return false;
    }

    // Common version is either type, they are identical.
    if (exact_match) {
      tc.type = tc.left->type();
      return true;
    }

    // Create a new array type. For now it is always const, but we will relax
    // this in the future. We'll need a second pass over the array types to
    // deduce whether const is needed to avoid the edge case in coerce_array().
    tc.type = innermost->unqualified();
    for (size_t i = 0; i < nlevels; i++) {
      if (innermost->isConst())
        tc.type = types_->newQualified(innermost, Qualifiers::Const);
      tc.type = types_->newArray(tc.type, ArrayType::kUnsized);
    }
    tc.left =
      new (pool_) sema::ImplicitCastExpr(tc.left_src, tc.type, sema::CastOp::None, tc.left);
    tc.right =
      new (pool_) sema::ImplicitCastExpr(tc.right_src, tc.type, sema::CastOp::None, tc.right);
    return true;
  }

  // If one type is an array and the other is not, abort.
  Type* left = tc.left->type();
  Type* right = tc.right->type();
  if (left_array || right_array) {
    cc_.report(tc.left_src->loc(), rmsg::no_common_conversion)
      << left << right;
    return false;
  }

  if (!CompareNonArrayTypesExactly(left->unqualified(),
                                   right->unqualified()))
  {
    cc_.report(tc.left_src->loc(), rmsg::no_common_conversion)
      << left << right;
    return false;
  }

  // Pick the "most const" type.
  if (left->isConst() && !right->isConst())
    tc.type = left;
  else if (right->isConst() && !left->isConst())
    tc.type = right;
  else
    tc.type = left;
  return true;
}

sema::Expr*
SemanticAnalysis::lvalue_to_rvalue(sema::LValueExpr* expr)
{
  // If the storage is indirectly addressable (i.e., it makes sense to take
  // the address of the l-value, and assign a new value to that address), then
  // we can perform a load operation to extract the actual value.
  //
  // If the storage is not indirectly addressable (such as fixed-length arrays),
  // then the r-value is the storage itself. We cannot compute an r-value in
  // this case, and what the caller probably wants is the array's address.
  Type* type = expr->type();
  if (type->isArray() && type->toArray()->hasFixedLength())
    return expr;

  // Peel away reference types, which can only exist on argument symbols.
  if (type->isReference())
    type = type->toReference()->inner();

  return new (pool_) sema::LoadExpr(expr->src(), type, expr);
}

bool
SemanticAnalysis::CompareNonArrayTypesExactly(Type* a, Type* b)
{
  if (a->qualifiers() != b->qualifiers())
    return false;

  a = a->unqualified();
  b = b->unqualified();

  if (a == b)
    return true;

  switch (a->canonicalKind()) {
    case Type::Kind::Function:
      if (!b->isFunction())
        return false;
      return AreFunctionTypesEqual(a->toFunction(), b->toFunction());
    case Type::Kind::Void:
    case Type::Kind::Unchecked:
    case Type::Kind::MetaFunction:
    case Type::Kind::Struct:
    case Type::Kind::Typeset:
    case Type::Kind::Enum:
    case Type::Kind::NullType:
    case Type::Kind::Primitive:
      // Handled by |a == b| above.
      return false;
    default:
      assert(false);
      return false;
  }
}

bool
SemanticAnalysis::no_conversion(EvalContext& ec)
{
  cc_.report(ec.from_src->loc(), rmsg::cannot_coerce) << ec.from->type() << ec.to;
  ec.result = nullptr;
  return false;
}

TestEvalContext::TestEvalContext(CompileContext& cc, sema::Expr* from)
 : EvalContext(CoercionKind::Test,
               from,
               cc.types()->getBool())
{
}

TestEvalContext::TestEvalContext(CompileContext& cc, ast::Expression* from_src)
 : EvalContext(CoercionKind::Test,
               from_src,
               cc.types()->getBool())
{
}

LValueToRValueContext::LValueToRValueContext(ast::Expression* from_src)
 : EvalContext(CoercionKind::RValue, from_src, nullptr)
{
}

LValueToRValueContext::LValueToRValueContext(sema::Expr* from)
 : EvalContext(CoercionKind::RValue, from, from->type())
{
}

} // namespace sp
