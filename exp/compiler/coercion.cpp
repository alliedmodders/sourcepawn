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
#include "coercion.h"

using namespace ke;
using namespace sp;

Coercion::Coercion(CompileContext &cc, Reason reason, Expression *expr, Type *to)
 : cc_(cc),
   pool_(cc.pool()),
   reason_(reason),
   expr_(expr),
   to_(to),
   out_(expr)
{
}

MessageBuilder
Coercion::diag(const SourceLocation &loc)
{
  if (message_)
    return MessageBuilder(message_);

  switch (result_) {
    case Result::discards_const_qualifiers:
      return cc_.note(loc, rmsg::coercion_diagnostic) << "discards const qualifiers";
    case Result::loses_precision:
      return cc_.note(loc, rmsg::coercion_diagnostic) << "destination type is less precise";
    case Result::ambiguous:
      return cc_.note(loc, rmsg::coercion_diagnostic) << "coercion is ambiguous";
    case Result::ref_is_readonly:
      return cc_.note(loc, rmsg::coercion_diagnostic) << "l-value is read-only";
    case Result::ref_expr_not_lvalue:
      return cc_.note(loc, rmsg::coercion_diagnostic) << "expression is not an l-value";
  }

  // No specific diagnostic.
  return MessageBuilder(nullptr);
}

auto
Coercion::coerce() -> Result
{
  // Step 1.1-1.n.
  if (to_->isReference()) {
    // References should never appear in the targets of shallow coercions or
    // assignments. Assignment operators always strip the reference away when
    // coercing.
    assert(reason_ == Reason::arg);

    return (result_ = coerceToRef());
  }

  // Steps 2.1-2.n.
  if (ReferenceType *from = expr_->type()->asReference()) {
    // Step 2.1
    Type *contained = from->contained();

    // Step 2.2.
    out_ = new (pool_) ImplicitCastExpr(out_, CastOp::deref, contained);
  }

  // Coercion, steps 3 and onward.
  return (result_ = coerceInner());
}

auto
Coercion::coerceInner() -> Result
{
  Type *from = expr_->type();
  switch (to_->canonicalKind()) {
    case Type::Kind::Struct:
      // Step 3.1.
      if (from->canonical() != to_->canonical())
        return Result::type_mismatch;

      // Step 3.2.
      if (from->isConst() && !to_->isConst())
        return Result::discards_const_qualifiers;

      // Step 3.3.
      return maybeCoerceToConst();

    case Type::Kind::Enum:
      // Steps 4.1-4.n.
      return coerceToEnum();

    case Type::Kind::Unchecked:
      // Steps 5.1-5.n.
      return coerceToUnchecked();

    case Type::Kind::Primitive:
    {
      PrimitiveType out = to_->semanticPrimitive();

      // Steps 6.1-6.4.
      if (out == PrimitiveType::Bool)
        return coerceToBool();

      // Steps 7.1-7.4.
      if (out == PrimitiveType::Float || out == PrimitiveType::Double)
        return coerceToFloat();

      // Steps 8.1-8.n.
      return coerceToInt();
    }

    case Type::Kind::Function:
      // Step 9.1.
      if (from->isNullType()) {
        out_ = new (pool_) ImplicitCastExpr(out_, CastOp::null_to_obj, to_);
        return Result::ok;
      }

      // Step 9.2.
      if (!from->isFunction())
        return Result::type_mismatch;

      // Steps 9.2-9.4.
      if (!AreFunctionTypesEqual(to_->toFunction(), from->toFunction()))
        return Result::type_mismatch;

      // Step 9.6.
      return Result::ok;

    case Type::Kind::Typeset:
      // Steps 10.1-10.n.
      return coerceToTypeset();

    case Type::Kind::Array:
      if (to_->toArray()->hasFixedLength()) {
        // Steps 11.1-11.n.
        return coerceToFixedArray();
      }

      // Steps 12.1-12.n.
      return coerceToArray();

    case Type::Kind::MetaFunction:
      // Step 13.1.
      if (from->isFunction()) {
        out_ = new (pool_) ImplicitCastExpr(out_, CastOp::to_metafunction, to_);
        return Result::ok;
      }

      // Step 13.2.
      if (from->isNullType()) {
        out_ = new (pool_) ImplicitCastExpr(out_, CastOp::null_to_obj, to_);
        return Result::ok;
      }

      // Step 13.3.
      if (from->isMetaFunction())
        return Result::ok;

      // Step 13.4.
      return Result::type_mismatch;

    case Type::Kind::NullType:
      // Step 14.1.
      if (from->isNullType())
        return Result::ok;

      // Step 14.2.
      return Result::type_mismatch;

    default:
      // We should never get here. Our parent strips out refs, we should never
      // get Unresolved past type resolution, and Quals/Typedefs aren't
      // canonical.
      assert(false);
      return Result::type_mismatch;
  }
}

auto
Coercion::coerceToRef() -> Result
{
  // Step 1.1.
  if (out_->vk() != VK::lvalue && out_->vk() != VK::clvalue)
    return Result::ref_expr_not_lvalue;

  Type *from = nullptr;
  if (ReferenceType *ref = out_->type()->asReference()) {
    assert(!ref->isConst());

    // Step 1.2.
    from = ref->contained();
  } else {
    // Step 1.3.
    from = out_->type();
  }

  // Step 1.4.
  Type *to = to_->toReference()->contained();

  // Step 1.5.1. If we have a const l-value, we can only coerce it to
  // references that won't modify its contents.
  if (out_->vk() == VK::clvalue && !to_->isConst()) {
    // Step 1.5.2.
    if (!to->isConst())
      return Result::ref_is_readonly;

    // Step 1.5.3.
    if (!to->hasMeaninglessConstCoercion())
      return Result::ref_is_readonly;
  }

  // Step 1.6.
  if (from->isConst() && !to->isConst())
    return Result::discards_const_qualifiers;

  // Step 1.7, part 1. Determine the const-qualifying context to use. |const|
  // on references is not transitive, so we use the inner const.
  Qualifiers qc = (to->qualifiers() & Qualifiers::Const);

  bool needsRefNopCast = false;

  // Step 1.7, part 2. Determine whether the types are equivalent.
  if (!AreTypesEquivalent(from->canonical(), to->canonical(), qc)) {
    // Step 1.7.1.
    if (!to->isUnchecked())
      return Result::type_mismatch;

    // Step 1.7.2.
    bool hasCoercionToUnchecked =
      to->isEnum() ||
      (to->isPrimitive() &&
       (to->primitive() == PrimitiveType::Int32 ||
        to->primitive() == PrimitiveType::Float));
    if (!hasCoercionToUnchecked)
      return Result::type_mismatch;

    // We have a cast from T& -> unchecked&, which is legal. We can't build
    // the cast op until we have a ref though, so we wait until the end.
    needsRefNopCast = true;
  }

  // Step 1.8. Build a reference if we don't have one yet.
  if (!out_->type()->isReference()) {
    Type *reftype = cc_.types()->newReference(from);
    if (out_->vk() == VK::clvalue)
      reftype = cc_.types()->newQualified(reftype, Qualifiers::Const);
    out_ = new (pool_) ImplicitCastExpr(out_, CastOp::lval_to_ref, reftype);
  }

  // Trivial casts from different reference types.
  if (needsRefNopCast)
    out_ = new (pool_) ImplicitCastExpr(out_, CastOp::ref_nop, to);

  // If either inner or outer const-qualifiers do not match, we need another
  // implicit cast. We just take the union of the inner and outer qualifiers
  // for now, since that effectively amounts to the same thing for all types.
  ReferenceType *reftype = out_->type()->toReference();
  if ((reftype->contained()->qualifiers() | reftype->qualifiers()) !=
     (to_->qualifiers() | to->qualifiers()))
  {
    out_ = new (pool_) ImplicitCastExpr(out_, CastOp::nop, to);
  }

  return Result::ok;
}

auto
Coercion::coerceToEnum() -> Result
{
  Type *from = out_->type();

  // Step 4.1.
  if (from->isNullType()) {
    if (MethodmapDecl *methodmap = to_->toEnum()->methodmap()) {
      if (methodmap->nullable()) {
        out_ = new (pool_) ImplicitCastExpr(out_, CastOp::null_to_enum, to_);
        return Result::ok;
      }
    }
  }

  // Step 4.2.
  if (reason_ == Reason::assign && from->isUnchecked()) {
    out_ = new (pool_) ImplicitCastExpr(out_, CastOp::bitcast, to_);
    return Result::ok;
  }

  // Step 4.3.
  if (to_->canonical() != from->canonical())
    return Result::type_mismatch;

  // Step 4.4.
  return Result::ok;
}

auto
Coercion::coerceToUnchecked() -> Result
{
  Type *from = out_->type();

  // Step 5.1.
  if (from->isUnchecked())
    return Result::ok;

  // Step 5.2.
  if (!from->isEnum() && !from->isPrimitive())
    return Result::type_mismatch;

  // Step 5.3.
  if (from->isEnum()) {
    out_ = new (pool_) ImplicitCastExpr(out_, CastOp::to_unchecked, to_);
    return Result::ok;
  }

  PrimitiveType fromType = from->semanticPrimitive();
  switch (fromType) {
    case PrimitiveType::Double:
    case PrimitiveType::Int64:
    case PrimitiveType::Uint64:
    case PrimitiveType::NativeInt:
    case PrimitiveType::NativeUint:
      // Step 5.4.
      return Result::type_mismatch;

    default:
      // Step 5.5-5.7.
      out_ = new (pool_) ImplicitCastExpr(out_, CastOp::to_unchecked, to_);
      return Result::ok;
  }
}

auto
Coercion::coerceToBool() -> Result
{
  Type *from = out_->type();

  // Step 6.1, part 1.
  if (from->isEnum()) {
    out_ = new (pool_) ImplicitCastExpr(out_, CastOp::int_to_bool, to_);
    return Result::ok;
  }

  // Step 6.1, part 2
  // Step 6.2-6.3.
  if (from->isPrimitive()) {
    switch (from->semanticPrimitive()) {
      case PrimitiveType::Int8:
      case PrimitiveType::Uint8:
      case PrimitiveType::Int16:
      case PrimitiveType::Uint16:
      case PrimitiveType::Int32:
      case PrimitiveType::Uint32:
      case PrimitiveType::Int64:
      case PrimitiveType::Uint64:
      case PrimitiveType::NativeInt:
      case PrimitiveType::NativeUint:
        out_ = new (pool_) ImplicitCastExpr(out_, CastOp::int_to_bool, to_);
        return Result::ok;

      case PrimitiveType::Float:
      case PrimitiveType::Double:
        out_ = new (pool_) ImplicitCastExpr(out_, CastOp::float_to_bool, to_);
        return Result::ok;

      case PrimitiveType::Bool:
        return Result::ok;
    }
  }

  // Step 6.4.
  return Result::type_mismatch;;
}

auto
Coercion::coerceToFloat() -> Result
{
  Type *from = out_->type();

  // Step 7.1.
  if (!from->isPrimitive())
    return Result::type_mismatch;

  switch (from->semanticPrimitive()) {
    // Step 7.2.
    case PrimitiveType::NativeInt:
    case PrimitiveType::NativeUint:
      return Result::type_mismatch;

    case PrimitiveType::Float:
      // Step 7.3.
      if (to_->primitive() == PrimitiveType::Double) {
        out_ = new (pool_) ImplicitCastExpr(out_, CastOp::float_to_double, to_);
        return Result::ok;
      }

      // Step 7.6.
      return Result::ok;

    case PrimitiveType::Double:
      // Step 7.4.
      if (to_->primitive() == PrimitiveType::Float)
        return Result::loses_precision;

      // Step 7.6.
      return Result::ok;

    case PrimitiveType::Int8:
    case PrimitiveType::Uint8:
    case PrimitiveType::Int16:
    case PrimitiveType::Uint16:
    case PrimitiveType::Int32:
    case PrimitiveType::Uint32:
    case PrimitiveType::Int64:
    case PrimitiveType::Uint64:
      // Step 7.5.
      out_ = new (pool_) ImplicitCastExpr(out_, CastOp::int_to_float, to_);
      return Result::ok;
  }

  // We'll get here through, say, bool.
  return Result::type_mismatch;
}

auto
Coercion::coerceToInt() -> Result
{
  Type *from = out_->type();
  PrimitiveType out = to_->semanticPrimitive();

  PrimitiveType in;
  if (!from->isPrimitive()) {
    // Step 8.1.
    if (!from->isUnchecked())
      return Result::type_mismatch;

    // Step 8.2.
    if (out != PrimitiveType::Int32 && out != PrimitiveType::Uint32)
      return Result::type_mismatch;

    // Change type to int32.
    in = PrimitiveType::Int32;
    out_ = new (pool_) ImplicitCastExpr(
      out_,
      CastOp::bitcast,
      cc_.types()->getPrimitive(PrimitiveType::Int32));
    from = out_->type();
  } else {
    in = from->semanticPrimitive();
  }

  // Step 8.3.
  if (in == PrimitiveType::Float || in == PrimitiveType::Double)
    return Result::ok;

  // Step 8.4.
  if (in == PrimitiveType::Bool)
    return Result::ok;

  // Step 8.5.
  if (out == PrimitiveType::NativeInt || out == PrimitiveType::NativeInt) {
    if (out != in)
      return Result::type_mismatch;
  }

  // Step 8.6.
  if (IsPrimitiveTypeSigned(out) != IsPrimitiveTypeSigned(in))
    return Result::type_mismatch;

  // Step 8.7.
  size_t inSize = SizeOfPrimitiveType(in);
  size_t outSize = SizeOfPrimitiveType(out);
  if (outSize < inSize)
    return Result::type_mismatch;

  // Step 8.8. This is so "char" and "int8" are not ambiguous.
  if (from->primitive() == PrimitiveType::Char &&
      to_->primitive() == PrimitiveType::Int8)
  {
    out_ = new (pool_) ImplicitCastExpr(out_, CastOp::bitcast, to_);
    return Result::ok;
  }

  // Step 8.9.
  if (outSize > inSize) {
    out_ = new (pool_) ImplicitCastExpr(out_, CastOp::promote_int, to_);
    return Result::ok;
  }

  // Step 8.10.
  return Result::ok;
}

auto
Coercion::coerceToTypeset() -> Result
{
  Type *from = out_->type();

  // Step 10.1.
  if (from->canonical() == to_->canonical()) {
    // Step 10.1.1.
    if (from->isConst() && !to_->isConst())
      return Result::discards_const_qualifiers;

    // Step 10.1.2.
    return maybeCoerceToConst();
  }

  // Step 10.2.
  if (reason_ == Reason::shallow)
    return Result::type_mismatch;

  TypesetType *typeset = to_->toTypeset();

  Vector<Expression *> candidates;
  size_t lastTypeIndex = INT_MAX;

  for (size_t i = 0; i < typeset->numTypes(); i++) {
    Type *selected = typeset->typeAt(i);

    // Try an argument coercion to the child type. This coercion is shallow so
    // we don't implicit construct a billion intermediary objects. Note that
    // we propagate the "out_" field and not "expr_" since we could have
    // already introduced implicit casts.
    Coercion candidate(cc_, Reason::shallow, out_, selected);

    // Attempt the coercion.
    if (candidate.coerce() != Result::ok)
      continue;

    Expression *result = candidate.output();
    if (result == out_) {
      // Step 10.3.1 - exact match.
      out_ = new (pool_) ConstructTypesetExpr(result->loc(), result, typeset, i);

      // Step 10.3.2.
      return maybeCoerceToConst();
    }

    // Some kind of coercion happened. Add to the candidate list.
    lastTypeIndex = i;
    candidates.append(out_);
  }

  // Step 10.4.
  if (candidates.length() > 1) {
    // Build a message anotating each possible coercion.
    MessageBuilder builder = cc_.reporting().build(out_->loc(), rmsg::ambiguous_coercion);
    for (size_t i = 0; i < candidates.length(); i++)
      builder << (cc_.note(out_->loc(), rmsg::typeset_candidate) << candidates[i]->type());
    message_ = builder.get();
    return Result::ambiguous;
  }

  // Step 10.5.
  if (candidates.length() == 0)
    return Result::type_mismatch;

  // Steps 10.6-10.6.
  assert(candidates.length() == 1);
  assert(lastTypeIndex < typeset->numTypes());
  out_ = new (pool_) ConstructTypesetExpr(out_->loc(), out_, typeset, lastTypeIndex);

  // Step 10.9.
  return maybeCoerceToConst();
}

auto
Coercion::coerceToFixedArray() -> Result
{
  // Step 11.1.
  if (!out_->type()->isArray() || !out_->type()->toArray()->hasFixedLength())
    return Result::type_mismatch;

  // Step 11.2.
  if (out_->type()->isConst() && !to_->isConst())
    return Result::discards_const_qualifiers;

  ArrayType *from = out_->type()->toArray();
  ArrayType *to = to_->toArray();

  // Steps 11.3 and 11.4.
  Type *innerFrom = from->contained();
  Type *innerTo = to->contained();

  // Step 11.5.
  if (!AreTypesEquivalent(innerFrom, innerTo, innerTo->qualifiers()))
    return Result::type_mismatch;

  // Step 11.6 and 11.7.
  int32_t fromSize = from->fixedLength();
  int32_t toSize = to->fixedLength();

  // Step 11.8.
  if (fromSize == toSize)
    return maybeCoerceToConst();

  // Step 11.9.
  if (reason_ == Reason::assign &&
      innerFrom->isPrimitive() &&
      innerFrom->primitive() == PrimitiveType::Char &&
      fromSize < toSize)
  {
    // Step 11.9.1. Note that the caller should have verified that const arrays
    // cannot be assigned.
    assert(!out_->type()->isConst());
    out_ = new (pool_) ImplicitCastExpr(out_, CastOp::char_array_extend, to);
    return Result::ok;
  }

  // Step 11.10.
  return Result::type_mismatch;
}

auto
Coercion::coerceToArray() -> Result
{
  // Step 12.1.
  if (!out_->type()->isArray())
    return Result::type_mismatch;

  // Step 12.2.
  if (out_->type()->isConst() && !to_->isConst())
    return Result::discards_const_qualifiers;

  ArrayType *from = out_->type()->toArray();
  ArrayType *to = to_->toArray();

  // Steps 12.3 and 12.4.
  Type *innerFrom = from->contained();
  Type *innerTo = to->contained();

  // Step 12.5.
  if (!AreTypesEquivalent(innerFrom, innerTo, innerTo->qualifiers()))
    return Result::type_mismatch;

  // Step 12.6.
  if (from->hasFixedLength()) {
    // Step 12.6.1 and 12.6.2.
    out_ = new (pool_) ImplicitCastExpr(out_, CastOp::fixed_array_decay, to);

    // Step 12.6.3.
    return maybeCoerceToConst();
  }

  // Step 12.7.
  return maybeCoerceToConst();
}

auto
Coercion::maybeCoerceToConst() -> Result
{
  if (!to_->isConst())
    return Result::ok;

  if (out_->type()->isConst())
    return Result::ok;

  // No operation is needed, but since we go from !const -> const, we need to
  // note that there was an implicit cast.
  out_ = new (pool_) ImplicitCastExpr(out_, CastOp::nop, to_);
  return Result::ok;
}
