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
#include "boxed-value.h"

using namespace ke;

Type ke::UnresolvedType(Type::Kind::Unresolved);

Type *
Type::NewVoid()
{
  Type *type = new (POOL()) Type(Kind::Void);
  return type;
}

Type *
Type::NewUnchecked()
{
  Type *type = new (POOL()) Type(Kind::Unchecked);
  return type;
}

Type *
Type::NewMetaFunction()
{
  return new (POOL()) Type(Kind::MetaFunction);
}

Type *
Type::NewPrimitive(PrimitiveType prim)
{
  Type *type = new (POOL()) Type(Kind::Primitive);
  type->primitive_ = prim;
  return type;
}

Type *
Type::NewQualified(Type *type, Qualifiers qualifiers)
{
  assert(!type->isQualified());
  Type *qual = new (POOL()) Type(Kind::Qualifier, type);
  qual->qualifiers_ = qualifiers;
  return qual;
}

ArrayType *
ArrayType::New(Type *contained, int elements)
{
  ArrayType *type = new (POOL()) ArrayType(Kind::Array);
  type->contained_ = contained;
  type->elements_ = elements;
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
  type->kind_ = Kind::Enum;
  type->name_ = name;
  return type;
}

TypedefType *
TypedefType::New(Atom *name, Type *canonical)
{
  assert(canonical);

  TypedefType *tdef = new (POOL()) TypedefType(name);
  tdef->canonical_ = canonical;
  tdef->actual_ = canonical;
  return tdef;
}

bool
Type::Compare(Type *left, Type *right)
{
  if (left == right)
    return true;

  if (left->kind_ != right->kind_)
    return false;

  switch (left->kind_) {
    case Kind::Primitive:
      return left->primitive() == right->primitive();

    case Kind::Array:
    {
      ArrayType *aleft = left->toArray();
      ArrayType *aright = right->toArray();
      return aleft->equalTo(aright);
    }

    case Kind::Function:
      // :TODO:
      return false;

    case Kind::Enum:
    case Kind::Typedef:
      return false;

    case Kind::Void:
      return true;

    default:
      assert(left->kind_ == Type::Kind::Reference);
      return Compare(left->toReference()->contained(), right->toReference()->contained());
  }
}

FunctionType *
FunctionType::New(FunctionSignature *sig)
{
  return new (POOL()) FunctionType(sig);
}

UnionType *
UnionType::New(Atom *name)
{
  return new (POOL()) UnionType(name);
}

StructType *
StructType::New(Atom *name)
{
  return new (POOL()) StructType(name);
}

const char *
ke::GetPrimitiveName(PrimitiveType type)
{
  switch (type) {
    case PrimitiveType::Bool:
      return "bool";
    case PrimitiveType::Char:
      return "char";
    case PrimitiveType::Int32:
      return "int";
    case PrimitiveType::Float:
      return "float";
    default:
      assert(false);
      return "unknown";
  }
}

const char *
ke::GetTypeName(Type *type)
{
  if (type->isUnresolved())
    return "unresolved";
  if (type->isArray())
    return "array";
  if (type->isFunction() || type->isMetaFunction())
    return "function";
  if (type->isVoid())
    return "void";
  if (type->isUnion())
    return type->toUnion()->name()->chars();
  if (type->isEnum())
    return type->toEnum()->name()->chars();
  if (type->isReference())
    type = type->toReference()->contained();
  return GetPrimitiveName(type->primitive());
}

const char *
ke::GetTypeClassName(Type *type)
{
  if (type->isUnresolved())
    return "unresolved";
  if (type->isArray())
    return "array";
  if (type->isFunction() || type->isMetaFunction())
    return "function";
  if (type->isVoid())
    return "void";
  if (type->isEnum())
    return "enum";
  if (type->isUnion())
    return "union";
  if (type->isReference())
    type = type->toReference()->contained();
  return GetPrimitiveName(type->primitive());
}

BoxedValue
ke::DefaultValueForPlainType(Type *type)
{
  if (type->isUnresolved())
    return BoxedValue(IntValue::FromInt32(0));
  if (type->isEnum())
    return BoxedValue(IntValue::FromInt32(0));
  switch (type->primitive()) {
    case PrimitiveType::Bool:
      return BoxedValue(false);
    case PrimitiveType::Char:
      return BoxedValue(IntValue::FromInt8(0));
    case PrimitiveType::Int32:
      return BoxedValue(IntValue::FromInt32(0));
    case PrimitiveType::Float:
      return BoxedValue(FloatValue::FromFloat(0));
  }
  assert(false);
}

int32_t
ke::ComputeSizeOfType(ReportingContext &cc, Type *aType, size_t level)
{
  if (!aType->isArray()) {
    cc.reportError(Message_SizeofRequiresArrayType);
    return 0;
  }

  ArrayType *type = aType->toArray();
  for (size_t i = 1; i <= level; i++) {
    if (!type->contained()->isArray()) {
      cc.reportError(Message_SizeofHasTooManyDimensions);
      return 0;
    }
    type = type->contained()->toArray();
  }

  if (!type->hasFixedSize()) {
    cc.reportError(Message_SizeofWithIndeterminateArray);
    return 0;
  }

  return type->fixedSize();
}
