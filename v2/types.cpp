// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2012-2014 David Anderson
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
#include "types.h"
#include "compile-context.h"

using namespace ke;

Type *
Type::NewVoid()
{
  Type *type = new (POOL()) Type(VOID);
  return type;
}

Type *
Type::NewUnchecked()
{
  Type *type = new (POOL()) Type(UNCHECKED);
  return type;
}

Type *
Type::NewPrimitive(PrimitiveType prim)
{
  Type *type = new (POOL()) Type(PRIMITIVE);
  type->primitive_ = prim;
  return type;
}

ArrayType *
ArrayType::New(Type *contained, int elements)
{
  assert(!contained->isArray() || contained->toArray()->levels() < MAX_ARRAY_DEPTH);

  ArrayType *type = new (POOL()) ArrayType(ARRAY);
  type->contained_ = contained;
  type->elements_ = elements;
  if (!contained->isArray())
    type->levels_ = 1;
  else
    type->levels_ = contained->toArray()->levels() + 1;
  return type;
}

ArrayType *
ArrayType::NewExternal(Type *contained)
{
  ArrayType *type = new (POOL()) ArrayType(EXTERNAL_ARRAY);

  assert(contained->isPrimitive());

  type->contained_ = contained;
  type->elements_ = DYNAMIC_ARRAY_SIZE;
  type->levels_ = 1;
  return type;
}

ReferenceType *
ReferenceType::New(Type *contained)
{
  ReferenceType *type = new (POOL()) ReferenceType();

  assert(!contained->isReference());

  type->contained_ = contained;
  return type;
}

EnumType *
EnumType::New(Atom *name)
{
  EnumType *type = new (POOL()) EnumType();
  type->kind_ = ENUM;
  type->name_ = name;
  return type;
}

#if 0
Type *
Type::NewNative(Type *returnType, Handle<FixedArray> parameters,
        Handle<FixedArray> defaults, bool variadic)
{
  Local<Type> type(zone, Type::cast(zone->allocate(MapKind_Type, sizeof(Type), Heap::Tenure_Old)));
  if (!type)
    return nullptr;

  type->kind_ = NATIVE;
  type->name_ = nullptr;
  type->contained_ = nullptr;
  type->fields_ = nullptr;
  type->newMap_ = nullptr;
  type->returnType_ = returnType;
  type->parameters_ = parameters;
  type->defaults_ = defaults;
  type->variadicNative_ = variadic;
  return type;
}
#endif

bool
Type::Compare(Type *left, Type *right)
{
  if (left == right)
    return true;

  if (left->kind() != right->kind())
    return false;

  switch (left->kind()) {
   case Type::PRIMITIVE:
    return left->primitive() == right->primitive();

   case Type::ARRAY:
    {
    ArrayType *aleft = left->toArray();
    ArrayType *aright = right->toArray();
    if (aleft->levels() != aright->levels())
      return false;
    if (aleft->isFixedLength() != aright->isFixedLength())
      return false;
    if (aleft->isFixedLength() && aleft->fixedLength() != aright->fixedLength())
      return false;
    return Compare(aleft->contained(), aright->contained());
   }

   case Type::FUNCTION:
    {
    FunctionType *fleft = left->toFunction();
    FunctionType *fright = right->toFunction();
    if (!Compare(fleft->returnType(), fright->returnType()))
      return false;
    if (fleft->parameters()->length() != fright->parameters()->length())
      return false;
    if (fleft->isNative() != fright->isNative())
      return false;
    if (fleft->isNative() && (fleft->isNativeVariadic() != fright->isNativeVariadic()))
      return false;
    for (unsigned i = 0; i < fleft->parameters()->length(); i++) {
      Type *leftparam = fleft->parameterAt(i);
      Type *rightparam = fright->parameterAt(i);
      if (!Compare(leftparam, rightparam))
        return false;
    }
    return true;
   }

   case Type::ENUM:
    return false;

   case Type::VOID:
    return true;

   default:
    assert(left->kind() == Type::REFERENCE);
    return Compare(left->toReference()->contained(), right->toReference()->contained());
  }
}

FunctionType *
FunctionType::New(TokenKind token)
{
  FunctionType *type = new (POOL()) FunctionType();
  type->kind_ = FUNCTION;
  type->parameters_ = nullptr;
  //type->defaults_ = nullptr;
  type->returnType_ = nullptr;
  type->token_ = token;
  return type;
}

const char *
ke::GetTypeName(Type *type)
{
  if (type->isArray())
    return "array";
  if (type->isFunction())
    return "function";
  if (type->isVoid())
    return "void";
  if (type->isEnum())
    return type->toEnum()->name()->chars();
  if (type->isReference())
    type = type->toReference()->contained();
  switch (type->primitive()) {
    case PrimitiveType_Bool:
    return "bool";
    case PrimitiveType_Char:
    return "String";
    case PrimitiveType_Int32:
    return "int";
    case PrimitiveType_Float:
    return "Float";
    default:
    return "<unknown>";
  }
}

