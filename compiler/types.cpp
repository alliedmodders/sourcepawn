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
#include "ast.h"

using namespace ke;
using namespace sp;

Type sp::UnresolvableType(Type::Kind::Unresolvable);

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
TypedefType::New(Atom *name)
{
  TypedefType *tdef = new (POOL()) TypedefType(name);
  tdef->actual_ = nullptr;
  return tdef;
}

void
TypedefType::resolve(Type *actual)
{
  canonical_ = actual;
  actual_ = actual;
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
sp::GetPrimitiveName(PrimitiveType type)
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

static const char *
GetBaseTypeName(Type *type)
{
  if (type->isUnresolvable())
    return "<unresolved>";
  if (type->isMetaFunction())
    return "function";
  if (type->isVoid())
    return "void";
  if (type->isUnion())
    return type->toUnion()->name()->chars();
  if (type->isEnum())
    return type->toEnum()->name()->chars();
  return GetPrimitiveName(type->primitive());
}

static AString BuildTypeFromSpecifier(const TypeSpecifier *spec);
static AString BuildTypeFromSignature(const FunctionSignature *sig);

static AString
BuildTypeFromSignature(const FunctionSignature *sig)
{
  AutoString base = "function ";
  base = base + BuildTypeFromSpecifier(sig->returnType());
  base = base + "(";

  for (size_t i = 0; i < sig->parameters()->length(); i++) {
    base = base + BuildTypeFromSpecifier(sig->parameters()->at(i)->spec());
    if (i != sig->parameters()->length() - 1)
      base = base + ", ";
  }
  base = base + ")";
  return AString(base.ptr());
}

static AString
BuildTypeFromSpecifier(const TypeSpecifier *spec)
{
  if (spec->resolved())
    return BuildTypeName(spec->resolved());

  AutoString base;
  switch (spec->resolver()) {
    case TOK_NAME:
    case TOK_LABEL:
      base = spec->proxy()->name()->chars();
      break;
    case TOK_FUNCTION:
      base = BuildTypeFromSignature(spec->signature());
      break;
    default:
      base = TokenNames[spec->resolver()];
      break;
  }

  for (size_t i = 0; i < spec->rank(); i++)
    base = base + "[]";

  return AString(base.ptr());
}

AString
sp::BuildTypeName(Type *aType)
{
  if (ArrayType *type = aType->asArray()) {
    Vector<ArrayType *> stack;

    Type *cursor = type;
    Type *innermost = nullptr;
    for (;;) {
      if (!cursor->isArray()) {
        innermost = cursor;
        break;
      }
      stack.append(cursor->toArray());
      cursor = cursor->toArray()->contained();
    }

    AutoString builder = BuildTypeName(innermost);
    for (size_t i = 0; i < stack.length(); i++) {
      if (stack[i]->hasFixedSize()) {
        builder = builder + "[]";
      } else {
        builder = builder + "[" + stack[i]->fixedSize() + "]";
      }
    }
    return AString(builder.ptr());
  }
  if (ReferenceType *type = aType->asReference()) {
    AutoString builder = BuildTypeName(type->contained());
    return AString((builder + "&").ptr());
  }
  if (FunctionType *type = aType->asFunction())
    return BuildTypeFromSignature(type->signature());

  return AString(GetBaseTypeName(aType));
}

const char *
sp::GetTypeName(Type *type)
{
  if (type->isUnresolvable())
    return "<unresolvable>";
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
sp::GetTypeClassName(Type *type)
{
  if (type->isUnresolvable())
    return "<unresolvable>";
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
sp::DefaultValueForPlainType(Type *type)
{
  if (type->isUnresolvable())
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
  return BoxedValue(IntValue::FromInt32(0));
}

int32_t
sp::ComputeSizeOfType(ReportingContext &cc, Type *aType, size_t level)
{
  if (aType->isUnresolvedTypedef()) {
    cc.report(rmsg::recursive_type);
    return 0;
  }
  if (!aType->isArray()) {
    cc.report(rmsg::sizeof_needs_array);
    return 0;
  }

  ArrayType *type = aType->toArray();
  for (size_t i = 1; i <= level; i++) {
    if (!type->contained()->isArray()) {
      cc.report(rmsg::sizeof_invalid_rank);
      return 0;
    }
    type = type->contained()->toArray();
  }

  if (!type->hasFixedSize()) {
    cc.report(rmsg::sizeof_indeterminate);
    return 0;
  }

  return type->fixedSize();
}
