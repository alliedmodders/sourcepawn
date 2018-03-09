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
#include "conversion.h"
#include <assert.h>

using namespace ke;
using namespace sp;

MessageBuilder
ConversionResult::diag(CompileContext& cc, const SourceLocation &loc)
{
  switch (error) {
    case ConversionError::discards_const_qualifiers:
      return cc.note(loc, rmsg::coercion_diagnostic) << "discards const qualifiers";
    case ConversionError::loses_precision:
      return cc.note(loc, rmsg::coercion_diagnostic) << "destination type is less precise";
    case ConversionError::ambiguous:
      return cc.note(loc, rmsg::coercion_diagnostic) << "coercion is ambiguous";
    case ConversionError::ref_is_readonly:
      return cc.note(loc, rmsg::coercion_diagnostic) << "l-value is read-only";
    case ConversionError::ref_expr_not_lvalue:
      return cc.note(loc, rmsg::coercion_diagnostic) << "expression is not an l-value";
    default:
      assert(false);
  }

  // No specific diagnostic.
  return MessageBuilder(nullptr);
}

ConversionResult
Conversion::Find(Type *from, Type *to)
{
  switch (to->canonicalKind()) {
    case Type::Kind::Struct:
    case Type::Kind::Typeset: // This is wrong, for now we don't care.
      // Step 1.1.
      if (from->canonical() != to->canonical())
        return ConversionResult(ConversionError::type_mismatch);

      // Step 1.2.
      if (from->isConst() && !to->isConst())
        return ConversionResult(ConversionError::discards_const_qualifiers);

      // Step 1.3.
      if (!from->isConst() && to->isConst())
        return ConversionResult(ConversionKind::trivial_const);
      return ConversionResult(ConversionKind::none);

    case Type::Kind::Enum:
      // Steps 2.1-2.n.
      return CoerceToEnum(from, to);

    case Type::Kind::Unchecked:
      // Steps 3.1-3.n.
      return CoerceToUnchecked(from, to);

    case Type::Kind::Primitive:
    {
      PrimitiveType out = to->primitive();

      // Steps 4.1-4.4.
      if (out == PrimitiveType::Bool)
        return CoerceToBool(from, to);

      // Steps 5.1-5.4.
      if (out == PrimitiveType::Float || out == PrimitiveType::Double)
        return CoerceToFloat(from, to);

      // Steps 6.1-6.n.
      return CoerceToInt(from, to);
    }

    case Type::Kind::Function:
      // Step 7.1.
      if (from->isNullType())
        return ConversionResult(ConversionKind::null_to_obj);

      // Step 7.2.
      if (!from->isFunction())
        return ConversionResult(ConversionError::type_mismatch);

      // Steps 7.2-7.4.
      if (!AreFunctionTypesEqual(to->toFunction(), from->toFunction()))
        return ConversionResult(ConversionError::type_mismatch);

      // Step 7.6.
      return ConversionResult(ConversionKind::none);

    case Type::Kind::Array:
      if (to->toArray()->hasFixedLength()) {
        // Steps 8.1-8.n.
        return CoerceToFixedArray(from, to);
      }

      // Steps 9.1-9.n.
      return CoerceToArray(from, to);

    case Type::Kind::NullType:
      // Step 10.1.
      if (from->isNullType())
        return ConversionResult(ConversionKind::none);

      // Step 10.2.
      return ConversionResult(ConversionError::type_mismatch);

    // This is an explicit set of steps until we have true class hierarchies.
    case Type::Kind::MetaFunction:
      // Step 11.1.
      if (from->isFunction())
        return ConversionResult(ConversionKind::downcast);

      // Step 11.2.
      if (from->isNullType())
        return ConversionResult(ConversionKind::null_to_obj);

      // Step 11.3.
      if (from->isMetaFunction())
        return ConversionResult(ConversionKind::none);

      // Step 11.4.
      return ConversionResult(ConversionError::type_mismatch);


    default:
      // We should never get here. Our parent strips out refs, we should never
      // get Unresolved past type resolution, and Quals/Typedefs aren't
      // canonical.
      assert(false);
      return ConversionResult(ConversionError::type_mismatch);
  }
}

bool
Conversion::IsCompatibleWithUnchecked(Type *t)
{
  return t->isInt32OrEnum() ||
         t->isBool() ||
         t->isChar() ||
         t->isFloat() ||
         t->isUnchecked();
}

ConversionResult
Conversion::CoerceToEnum(Type *from, Type *to)
{
  // Step 2.1.
  if (from->isNullType()) {
    if (MethodmapDecl *methodmap = to->toEnum()->methodmap()) {
      if (methodmap->nullable())
        return ConversionResult(ConversionKind::null_to_int);
    }
  }

  // Step 2.2.
  if (from->isUnchecked())
    return ConversionResult(ConversionKind::bitcast);

  // Step 2.3.
  if (to->canonical() != from->canonical())
    return ConversionResult(ConversionError::type_mismatch);

  // Step 2.4.
  return ConversionResult(ConversionKind::none);
}

ConversionResult
Conversion::CoerceToUnchecked(Type *from, Type *to)
{
  // Step 3.1.
  if (from->isUnchecked())
    return ConversionResult(ConversionKind::none);

  // Step 3.2.
  if (from->isEnum())
    return ConversionResult(ConversionKind::bitcast);

  // Step 3.3.
  if (!from->isPrimitive())
    return ConversionResult(ConversionError::type_mismatch);

  PrimitiveType fromType = from->primitive();
  switch (fromType) {
    case PrimitiveType::Double:
    case PrimitiveType::Int64:
    case PrimitiveType::Uint64:
    case PrimitiveType::NativeInt:
    case PrimitiveType::NativeUint:
      // Step 3.4.
      return ConversionResult(ConversionError::type_mismatch);

    default:
      // Step 3.5-3.8.
      return ConversionResult(ConversionKind::to_unchecked);
  }
}

ConversionResult
Conversion::CoerceToBool(Type *from, Type *to)
{
  // Step 4.1, part 1.
  if (from->isEnum())
    return ConversionResult(ConversionKind::to_bool);

  // Step 4.1, part 2
  // Step 4.2-6.3.
  if (from->isPrimitive()) {
    switch (from->primitive()) {
      case PrimitiveType::Char:
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
        return ConversionResult(ConversionKind::to_bool);

      case PrimitiveType::Float:
      case PrimitiveType::Double:
        return ConversionResult(ConversionKind::to_bool);

      case PrimitiveType::Bool:
        return ConversionResult(ConversionKind::none);
    }
  }

  // Step 4.4.
  return ConversionResult(ConversionError::type_mismatch);
}

ConversionResult
Conversion::CoerceToFloat(Type *from, Type *to)
{
  // Steps 5.1.1-5.1.n.
  if (from->isUnchecked()) {
    // Step 5.1.1.
    if (to->isFloat())
      return ConversionResult(ConversionKind::bitcast);

    // Step 5.1.2.
    return ConversionResult(ConversionError::type_mismatch);
  }

  // Step 5.2.
  if (!from->isPrimitive())
    return ConversionResult(ConversionError::type_mismatch);

  switch (from->primitive()) {
    // Step 5.3.
    case PrimitiveType::NativeInt:
    case PrimitiveType::NativeUint:
      return ConversionResult(ConversionError::type_mismatch);

    case PrimitiveType::Float:
      // Step 5.4.
      if (to->primitive() == PrimitiveType::Double)
        return ConversionResult(ConversionKind::float_to_double);

      // Step 5.5.
      return ConversionResult(ConversionKind::none);

    case PrimitiveType::Double:
      // Step 5.6.
      if (to->primitive() == PrimitiveType::Float)
        return ConversionResult(ConversionError::loses_precision);

      // Step 5.7.
      return ConversionResult(ConversionKind::none);

    case PrimitiveType::Int8:
    case PrimitiveType::Uint8:
    case PrimitiveType::Int16:
    case PrimitiveType::Uint16:
    case PrimitiveType::Int32:
    case PrimitiveType::Uint32:
    case PrimitiveType::Int64:
    case PrimitiveType::Uint64:
      return ConversionResult(ConversionKind::int_to_float);
  }

  // Step 5.8. We'll get here through, say, bool or char.
  return ConversionResult(ConversionError::type_mismatch);
}

ConversionResult
Conversion::CoerceToInt(Type *from, Type *to)
{
  PrimitiveType out = to->primitive();

  if (from->isUnchecked()) {
    // Step 6.1.1.
    if (out == PrimitiveType::Int32)
      return ConversionResult(ConversionKind::bitcast);

    // Step 6.1.2.
    return ConversionResult(ConversionError::type_mismatch);
  }

  // Step 6.2.
  if (!from->isPrimitive())
    return ConversionResult(ConversionError::type_mismatch);

  PrimitiveType in = from->primitive();

  // Step 6.3.
  if (in == PrimitiveType::Float || in == PrimitiveType::Double)
    return ConversionResult(ConversionError::loses_precision);

  // Step 6.4.
  if (in == PrimitiveType::Bool)
    return ConversionResult(ConversionError::type_mismatch);

  // Step 6.5.
  if (out == PrimitiveType::NativeInt || out == PrimitiveType::NativeInt) {
    if (out != in)
      return ConversionResult(ConversionError::type_mismatch);
  }

  // Step 6.6.
  if (IsPrimitiveTypeSigned(out) != IsPrimitiveTypeSigned(in))
    return ConversionResult(ConversionError::type_mismatch);

  // Step 6.7. This stops 'int8' and 'char' overloads from being ambiguous.
  if (in == PrimitiveType::Int8 && out == PrimitiveType::Char)
    return ConversionResult(ConversionKind::bitcast);

  // Step 6.8.
  size_t inSize = SizeOfPrimitiveType(in);
  size_t outSize = SizeOfPrimitiveType(out);
  if (inSize > outSize)
    return ConversionResult(ConversionError::type_mismatch);

  // Step 6.9.
  if (outSize > inSize)
    return ConversionResult(ConversionKind::promote_int);

  // Step 6.10.
  return ConversionResult(ConversionKind::none);
}

ConversionResult
Conversion::CoerceToFixedArray(Type *aStart, Type *aDest)
{
  // Step 8.1.
  if (!aStart->isArray() || !aStart->toArray()->hasFixedLength())
    return ConversionResult(ConversionError::type_mismatch);

  // Step 8.2.
  if (aStart->isConst() && !aDest->isConst())
    return ConversionResult(ConversionError::discards_const_qualifiers);

  ArrayType *from = aStart->toArray();
  ArrayType *to = aDest->toArray();

  // Steps 8.3 and 8.4.
  Type *innerFrom = from->contained();
  Type *innerTo = to->contained();

  // Step 8.5.
  if (!AreTypesEquivalent(innerFrom, innerTo, innerTo->qualifiers()))
    return ConversionResult(ConversionError::type_mismatch);

  // Step 8.6 and 8.7.
  int32_t fromSize = from->fixedLength();
  int32_t toSize = to->fixedLength();

  // Step 8.8.
  if (fromSize == toSize) {
    if (!aStart->isConst() && aDest->isConst())
      return ConversionResult(ConversionKind::trivial_const);
    return ConversionResult(ConversionKind::none);
  }

  // Step 8.9.
  if (innerFrom->isPrimitive() &&
      innerFrom->primitive() == PrimitiveType::Char &&
      fromSize < toSize)
  {
    // Step 8.9.1. Note that the caller should have verified that const arrays
    // cannot be assigned.
    assert(!aStart->isConst());
    return ConversionResult(ConversionKind::char_array_extend);
  }

  // Step 8.10.
  return ConversionResult(ConversionError::type_mismatch);
}

ConversionResult
Conversion::CoerceToArray(Type *aStart, Type *aDest)
{
  // Step 9.1.
  if (!aStart->isArray())
    return ConversionResult(ConversionError::type_mismatch);

  // Step 9.2.
  if (aStart->isConst() && !aDest->isConst())
    return ConversionResult(ConversionError::discards_const_qualifiers);

  ArrayType *from = aStart->toArray();
  ArrayType *to = aDest->toArray();

  // Steps 9.3 and 9.4.
  Type *innerFrom = from->contained();
  Type *innerTo = to->contained();

  // Step 9.5.
  if (!AreTypesEquivalent(innerFrom, innerTo, innerTo->qualifiers()))
    return ConversionResult(ConversionError::type_mismatch);

  // Step 9.6.
  if (from->hasFixedLength()) {
    // Step 9.6.1 and 9.6.2.
    return ConversionResult(ConversionKind::fixed_array_decay);
  }

  // Step 9.7.
  if (!aStart->isConst() && aDest->isConst())
    return ConversionResult(ConversionKind::trivial_const);
  return ConversionResult(ConversionKind::none);
}
