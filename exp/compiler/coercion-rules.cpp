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
#include "conversion.h"

using namespace ke;
using namespace sp;

MessageBuilder
SemanticAnalysis::diag(const SourceLocation &loc, CR status)
{
  switch (status) {
    case CR::discards_const_qualifiers:
      return cc_.note(loc, rmsg::coercion_diagnostic) << "discards const qualifiers";
    case CR::loses_precision:
      return cc_.note(loc, rmsg::coercion_diagnostic) << "destination type is less precise";
    case CR::ambiguous:
      return cc_.note(loc, rmsg::coercion_diagnostic) << "coercion is ambiguous";
    case CR::ref_is_readonly:
      return cc_.note(loc, rmsg::coercion_diagnostic) << "l-value is read-only";
    case CR::ref_expr_not_lvalue:
      return cc_.note(loc, rmsg::coercion_diagnostic) << "expression is not an l-value";
  }

  // No specific diagnostic.
  return MessageBuilder(nullptr);
}

auto
SemanticAnalysis::do_coerce(Coercion &cr) -> CR
{
  CR result = try_coerce(cr, cr.from(), cr.to);
  if (result == CR::ok || result == CR::converted)
    cr.expr->setOp(cr.op);
  return result;
}

auto
SemanticAnalysis::try_coerce(Coercion &cr, Type *from, Type *to) -> CR
{
  // Step 1.1-1.n.
  if (to->isReference()) {
    // References should never appear in the targets of shallow coercions or
    // assignments. Assignment operators always strip the reference away when
    // coercing.
    assert(cr.context == Coercion::arg);

    return coerceToRef(cr, from, to);
  }

  // Steps 2.1-2.n.
  if (from->isReference()) {
    // Step 2.1
    from = from->toReference()->contained();

    // Step 2.2.
    cr.op = new (pool_) DerefOp(cr.op, from);
  }

  // Coercion, steps 3 and onward.
  return coerceInner(cr, from, to);
}

auto
SemanticAnalysis::coerceInner(Coercion &cr, Type *from, Type *to) -> CR
{
  switch (to->canonicalKind()) {
    case Type::Kind::Struct:
      // Step 3.1.
      if (from->canonical() != to->canonical())
        return CR::cant_coerce_to_struct;

      // Step 3.2.
      if (from->isConst() && !to->isConst())
        return CR::discards_const_qualifiers;

      // Step 3.3.
      return maybeCoerceToConst(cr, CR::ok, to);

    case Type::Kind::Enum:
      // Step 4.1.
      if (from->isNullType()) {
        if (MethodmapDecl *methodmap = to->toEnum()->methodmap()) {
          if (methodmap->nullable()) {
            cr.op = new (pool_) NullCastOp(cr.op, to);
            return CR::converted;
          }
        }
      }

      // Step 4.2.
      if (cr.context == Coercion::assign && from->isUnchecked()) {
        cr.op = new (pool_) UncheckedToTypedOp(cr.op, to);
        return CR::converted;
      }

      // Step 4.3.
      if (to->canonical() != from->canonical())
        return CR::enum_type_mismatch;

      // Step 4.4.
      return CR::ok;

    case Type::Kind::Unchecked:
    {
      // Step 5.1.
      if (from->isUnchecked())
        return CR::ok;

      // Step 5.2.
      if (!from->isEnum() && !from->isPrimitive())
        return CR::unchecked_type_mismatch;

      // Step 5.3.
      if (from->isEnum()) {
        cr.op = new (pool_) ToUncheckedOp(cr.op, to);
        return CR::converted;
      }

      PrimitiveType fromType = from->semanticPrimitive();
      switch (fromType) {
        case PrimitiveType::Double:
        case PrimitiveType::Int64:
        case PrimitiveType::Uint64:
        case PrimitiveType::NativeInt:
        case PrimitiveType::NativeUint:
          // Step 5.4.
          return CR::unchecked_type_mismatch;

        default:
          // Step 5.5-5.7.
          cr.op = new (pool_) ToUncheckedOp(cr.op, to);
          return CR::converted;
      }
    }

    case Type::Kind::Primitive:
    {
      PrimitiveType out = to->semanticPrimitive();

      // Steps 6.1-6.4.
      if (out == PrimitiveType::Bool)
        return coerceToBool(cr, from, to);

      // Steps 7.1-7.4.
      if (out == PrimitiveType::Float || out == PrimitiveType::Double)
        return coerceToFloat(cr, from, to);

      // Steps 8.1-8.n.
      return coerceToInt(cr, from, to);
    }

    case Type::Kind::Function:
      // Step 9.1.
      if (from->isNullType()) {
        cr.op = new (pool_) NullCastOp(cr.op, to);
        return CR::ok;
      }

      // Step 9.2.
      if (!from->isFunction())
        return CR::cant_coerce_to_function;

      // Steps 9.2-9.4.
      if (!AreFunctionTypesEqual(to->toFunction(), from->toFunction()))
        return CR::function_type_mismatch;

      // Step 9.6.
      return CR::ok;

    case Type::Kind::Typeset:
      // Steps 10.1-10.n. Note that we do not call toTypeset() since this
      // would lose const-qualifiers.
      return coerceToTypeset(cr, from, to);

    case Type::Kind::Array:
      if (to->toArray()->hasFixedLength()) {
        // Steps 11.1-11.n.
        return coerceToFixedArray(cr, from, to);
      }

      // Steps 12.1-12.n.
      return coerceToArray(cr, from, to);

    case Type::Kind::MetaFunction:
      // Step 13.1.
      if (from->isFunction()) {
        // No actual conversion takes place, but we need to note that this is
        // not an exact match.
        return CR::converted;
      }

      // Step 13.2.
      if (from->isNullType())
        return CR::converted;

      // Step 13.3.
      if (from->isMetaFunction())
        return CR::ok;

      // Step 13.4.
      return CR::cant_coerce_to_metafunction;

    case Type::Kind::NullType:
      // Step 14.1.
      if (from->isNullType())
        return CR::ok;

      // Step 14.2.
      return CR::cant_coerce_to_null;

    default:
      // We should never get here. Our parent strips out refs, we should never
      // get Unresolved past type resolution, and Quals/Typedefs aren't
      // canonical.
      assert(false);
      return CR::unexpected_type;
  }
}

auto
SemanticAnalysis::coerceToRef(Coercion &cr, Type *aFrom, Type *aTo) -> CR
{
  // Step 1.1.
  if (cr.vk() != VK::lvalue && cr.vk() != VK::clvalue)
    return CR::ref_expr_not_lvalue;

  Type *from = nullptr;

  if (ReferenceType *ref = aFrom->asReference()) {
    assert(!ref->isConst());

    // Step 1.2.
    from = ref->contained();
  } else {
    // Step 1.3.
    from = aFrom;
  }

  // Step 1.4.
  Type *to = aTo->toReference()->contained();

  // Step 1.5.1. If we have a const l-value, we can only coerce it to
  // references that won't modify its contents.
  if (cr.vk() == VK::clvalue && !aTo->isConst()) {
    // Step 1.5.2.
    if (!to->isConst())
      return CR::ref_is_readonly;

    // Step 1.5.3.
    if (!to->hasMeaninglessConstCoercion())
      return CR::ref_is_readonly;
  }

  // Step 1.6.
  if (from->isConst() && !to->isConst())
    return CR::discards_const_qualifiers;

  // Step 1.7, part 1. Determine the const-qualifying context to use. |const|
  // on references is not transitive, so we use the inner const.
  Qualifiers qc = (to->qualifiers() & Qualifiers::Const);

  // Keep track of whether we needed an implicit cast.
  CR status = CR::ok;

  // Step 1.7, part 2. Determine whether the types are equivalent.
  if (!AreTypesEquivalent(from->canonical(), to->canonical(), qc)) {
    // Step 1.7.1.
    if (!to->isUnchecked())
      return CR::ref_inner_type_mismatch;

    // Step 1.7.2.
    bool hasCoercionToUnchecked =
      to->isEnum() ||
      (to->isPrimitive() &&
       (to->primitive() == PrimitiveType::Int32 ||
        to->primitive() == PrimitiveType::Float));
    if (!hasCoercionToUnchecked)
      return CR::ref_inner_type_mismatch;

    // Just mark this as converted. We don't actually build a new reference.
    status = CR::converted;
  }

  // Step 1.8.
  status = maybeCoerceToConst(cr, status, to);

  // Step 1.9.
  if (!aFrom->isReference())
    cr.op = new (pool_) LvalToRefOp(cr.op, aTo);

  return status;
}

auto
SemanticAnalysis::coerceToBool(Coercion &cr, Type *from, Type *to) -> CR
{
  // Step 6.1, part 1.
  if (from->isEnum()) {
    cr.op = new (pool_) ToBoolOp(cr.op, to);
    return CR::converted;
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
      case PrimitiveType::Float:
      case PrimitiveType::Double:
        cr.op = new (pool_) ToBoolOp(cr.op, to);
        return CR::converted;

      case PrimitiveType::Bool:
        return CR::ok;
    }
  }

  // Step 6.4.
  return CR::cant_coerce_to_bool;
}

auto
SemanticAnalysis::coerceToFloat(Coercion &cr, Type *from, Type *to) -> CR
{
  // Step 7.1.
  if (!from->isPrimitive())
    return CR::cant_coerce_to_float;

  switch (from->semanticPrimitive()) {
    // Step 7.2.
    case PrimitiveType::NativeInt:
    case PrimitiveType::NativeUint:
      return CR::cant_coerce_to_float;

    case PrimitiveType::Float:
      // Step 7.3.
      if (to->primitive() == PrimitiveType::Double) {
        cr.op = new (pool_) FloatToDoubleOp(cr.op, to);
        return CR::converted;
      }

      // Step 7.6.
      return CR::ok;

    case PrimitiveType::Double:
      // Step 7.4.
      if (to->primitive() == PrimitiveType::Float)
        return CR::loses_precision;

      // Step 7.6.
      return CR::ok;

    case PrimitiveType::Int8:
    case PrimitiveType::Uint8:
    case PrimitiveType::Int16:
    case PrimitiveType::Uint16:
    case PrimitiveType::Int32:
    case PrimitiveType::Uint32:
    case PrimitiveType::Int64:
    case PrimitiveType::Uint64:
      // Step 7.5.
      cr.op = new (pool_) IntToFloatOp(cr.op, to);
      return CR::converted;
  }

  // Should not get here.
  return CR::cant_coerce_to_float;
}

auto
SemanticAnalysis::coerceToInt(Coercion &cr, Type *from, Type *to) -> CR
{
  PrimitiveType out = to->semanticPrimitive();

  PrimitiveType in;
  if (!from->isPrimitive()) {
    // Step 8.1.
    if (!from->isUnchecked())
      return CR::cant_coerce_to_int;

    // Step 8.2.
    if (out != PrimitiveType::Int32 && out != PrimitiveType::Uint32)
      return CR::cant_coerce_to_int;

    in = PrimitiveType::Int32;
  } else {
    in = from->semanticPrimitive();
  }

  // Step 8.3.
  if (in == PrimitiveType::Float || in == PrimitiveType::Double)
    return CR::cant_coerce_to_int;

  // Step 8.4.
  if (in == PrimitiveType::Bool)
    return CR::cant_coerce_to_int;

  // Step 8.5.
  if (out == PrimitiveType::NativeInt || out == PrimitiveType::NativeInt) {
    if (out != PrimitiveType::NativeInt && out != PrimitiveType::NativeInt)
      return CR::cant_coerce_to_int;
  }

  // Step 8.6.
  size_t inSize = SizeOfPrimitiveType(in);
  size_t outSize = SizeOfPrimitiveType(out);
  if (outSize < inSize)
    return CR::cant_coerce_to_int;

  // Steps 8-7-8.9.
  if (in != out) {
    cr.op = new (pool_) IntegerCastOp(cr.op, to);
    return CR::converted;
  }

  // Step 8.10.
  return CR::ok;
}

auto
SemanticAnalysis::coerceToTypeset(Coercion &aCoercion, Type *from, Type *aTarget) -> CR
{
  // Step 10.1.
  if (from->canonical() == aTarget->canonical()) {
    // Step 10.1.1.
    if (from->isConst() && !aTarget->isConst())
      return CR::discards_const_qualifiers;

    // Step 10.1.2.
    return maybeCoerceToConst(aCoercion, CR::ok, aTarget);
  }

  // Step 10.2.
  if (aCoercion.context == Coercion::shallow)
    return CR::cant_coerce_to_typeset;

  TypesetType *to = aTarget->toTypeset();

  Vector<Coercion> candidates;
  size_t first_candidate = INT_MAX;

  for (size_t i = 0; i < to->numTypes(); i++) {
    Type *selected = to->typeAt(i);

    // Try an argument coercion to the child type. This coercion is shallow so
    // we don't implicit construct a billion intermediary objects.
    Coercion candidate(cc_, aCoercion.expr, Coercion::shallow, selected);

    // Propagate the op field so the current type is accurate.
    candidate.op = aCoercion.op;

    // Attempt the coercion.
    switch (try_coerce(candidate, candidate.from(), candidate.to)) {
      case CR::ok:
        // Step 10.3.1 - exact match.
        aCoercion.op = new (pool_) ToTypesetOp(aCoercion.op, i, to);

        // Step 10.3.2.
        return maybeCoerceToConst(aCoercion, CR::converted, to);

      case CR::converted:
        // Coercion may be ambiguous, so we add it to a list.
        candidates.append(candidate);
        if (candidates.length() == 1)
          first_candidate = i;
        break;
    }
  }

  // Step 10.4.
  if (candidates.length() > 1) {
    // We report a diagnostic message here.
    ReportingContext &rr = aCoercion.rr;
    MessageBuilder builder = rr.build(rmsg::ambiguous_coercion);
    for (size_t i = 0; i < candidates.length(); i++) {
      // Annotate each possible coercion.
      builder << (rr.note(rmsg::typeset_candidate) << candidates[i].to);
    }

    aCoercion.message = builder.get();
    return CR::ambiguous;
  }

  // Step 10.5.
  if (candidates.length() == 0) {
    return CR::cant_coerce_to_typeset;
  }

  // Steps 10.6-10.8.
  assert(candidates.length() == 1);
  assert(first_candidate < to->numTypes());
  Coercion &source = candidates[0];
  aCoercion.op = new (pool_) ToTypesetOp(source.op, first_candidate, to);

  // Step 10.9.
  return maybeCoerceToConst(aCoercion, CR::converted, to);
}

auto
SemanticAnalysis::coerceToFixedArray(Coercion &cr, Type *aFrom, Type *aTo) -> CR
{
  // Step 11.1.
  if (!aFrom->isArray() || !aFrom->toArray()->hasFixedLength())
    return CR::cant_coerce_to_fixedarray;

  // Step 11.2.
  if (aFrom->isConst() && !aTo->isConst())
    return CR::discards_const_qualifiers;

  ArrayType *from = aFrom->toArray();
  ArrayType *to = aTo->toArray();

  // Steps 11.3 and 11.4.
  Type *innerFrom = from->contained();
  Type *innerTo = to->contained();

  // Step 11.5.
  if (!AreTypesEquivalent(innerFrom, innerTo, innerTo->qualifiers()))
    return CR::cant_coerce_to_fixedarray;

  // Step 11.6 and 11.7.
  int32_t fromSize = from->fixedLength();
  int32_t toSize = to->fixedLength();

  // Step 11.8.
  if (fromSize == toSize)
    return maybeCoerceToConst(cr, CR::ok, aTo);

  // Step 11.9.
  if (cr.context == Coercion::assign &&
      innerFrom->isPrimitive() &&
      innerFrom->primitive() == PrimitiveType::Char &&
      fromSize < toSize)
  {
    // Step 11.9.1. We don't add an operation here (we rely on CG doing the
    // right thing), but we do need to return "converted" to indicate this
    // was not an exact match.
    return CR::converted;
  }

  // Step 11.10.
  return CR::cant_coerce_to_fixedarray;
}

auto
SemanticAnalysis::coerceToArray(Coercion &cr, Type *aFrom, Type *aTo) -> CR
{
  // Step 12.1.
  if (!aFrom->isArray())
    return CR::cant_coerce_to_array;

  // Step 12.2.
  if (aFrom->isConst() && !aTo->isConst())
    return CR::discards_const_qualifiers;

  ArrayType *from = aFrom->toArray();
  ArrayType *to = aTo->toArray();

  // Steps 12.3 and 12.4.
  Type *innerFrom = from->contained();
  Type *innerTo = to->contained();

  // Step 12.5.
  if (!AreTypesEquivalent(innerFrom, innerTo, innerTo->qualifiers()))
    return CR::cant_coerce_to_fixedarray;

  // Step 12.6.
  if (from->hasFixedLength()) {
    // Step 12.6.1 and 12.6.2.
    cr.op = new (pool_) FixedArrayDecayOp(cr.op, to);

    // Step 12.6.3.
    return maybeCoerceToConst(cr, CR::converted, to);
  }

  // Step 12.7.
  return maybeCoerceToConst(cr, CR::ok, to);
}

auto
SemanticAnalysis::maybeCoerceToConst(Coercion &cr, CR status, Type *to) -> CR
{
  if (!to->isConst())
    return status;

  Type *from = cr.from();
  if (from->isConst())
    return status;

  // No operation is needed, but since we go from !const -> const, we need to
  // note that there was an implicit cast.
  return CR::converted;
}
