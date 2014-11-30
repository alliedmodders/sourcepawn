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

#include <string.h>
#include "type-manager.h"

using namespace ke;
using namespace sp;

TypeManager::TypeManager(StringPool &strings)
 : strings_(strings),
   voidType_(nullptr),
   uncheckedType_(nullptr),
   metaFunctionType_(nullptr),
   primitiveTypes_(),
   referenceTypes_()
{
  atom_String_ = strings.add("String");
  atom_Float_ = strings.add("Float");
  atom_any_ = strings.add("any");
  atom_Function_ = strings.add("Function");
  atom_bool_ = strings.add("bool");
}

bool
TypeManager::initialize()
{
  voidType_ = Type::NewVoid();
  uncheckedType_ = Type::NewUnchecked();
  metaFunctionType_ = Type::NewMetaFunction();

  primitiveTypes_[size_t(PrimitiveType::ImplicitInt)] = Type::NewPrimitive(PrimitiveType::ImplicitInt);
  primitiveTypes_[size_t(PrimitiveType::Int32)] = Type::NewPrimitive(PrimitiveType::Int32);
  primitiveTypes_[size_t(PrimitiveType::Float)] = Type::NewPrimitive(PrimitiveType::Float);
  primitiveTypes_[size_t(PrimitiveType::Char)] = Type::NewPrimitive(PrimitiveType::Char);
  primitiveTypes_[size_t(PrimitiveType::Bool)] = Type::NewPrimitive(PrimitiveType::Bool);

  for (size_t i = 0; i < kTotalPrimitiveTypes; i++)
    referenceTypes_[i] = ReferenceType::New(primitiveTypes_[i]);

  return true;
}

ArrayType *
TypeManager::newArray(Type *contained, int elements)
{
  // :TODO: cache this.
  return ArrayType::New(contained, elements);
}

ReferenceType *
TypeManager::newReference(Type *type)
{
  if (type->isPrimitive())
    return referenceTypes_[size_t(type->primitive())];

  // :TODO: cache this.
  return ReferenceType::New(type);
}

EnumType *
TypeManager::newEnum(Atom *name)
{
  return EnumType::New(name);
}

Type *
TypeManager::newQualified(Type *type, Qualifiers qualifiers)
{
  if ((type->qualifiers() | qualifiers) == qualifiers)
    return type;

  if (type->isQualified()) {
    qualifiers |= type->qualifiers();
    type = type->unqualified();
  }

  // :TODO: cache this.
  return Type::NewQualified(type, qualifiers);
}

UnionType *
TypeManager::newUnion(Atom *name)
{
  return UnionType::New(name);
}

StructType *
TypeManager::newStruct(Atom *name)
{
  return StructType::New(name);
}

TypedefType *
TypeManager::newTypedef(Atom *name)
{
  return TypedefType::New(name);
}

Type *
TypeManager::typeForLabelAtom(Atom *atom)
{
  if (atom == atom_String_)
    return getPrimitive(PrimitiveType::Char);
  if (atom == atom_Float_)
    return getPrimitive(PrimitiveType::Float);
  if (atom == atom_any_)
    return getUnchecked();
  if (atom == atom_Function_)
    return getMetaFunction();
  if (atom == atom_bool_)
    return getPrimitive(PrimitiveType::Bool);
  return nullptr;
}
