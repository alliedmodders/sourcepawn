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

TypeManager::TypeManager()
{
}

bool
TypeManager::initialize()
{
  voidType_ = Type::NewVoid();
  if (!voidType_)
    return false;

  uncheckedType_ = Type::NewUnchecked();
  if (!uncheckedType_)
    return false;

  if ((primitiveTypes_[size_t(PrimitiveType::Int32)] = Type::NewPrimitive(PrimitiveType::Int32)) == nullptr)
    return false;
  if ((primitiveTypes_[size_t(PrimitiveType::Float)] = Type::NewPrimitive(PrimitiveType::Float)) == nullptr)
    return false;
  if ((primitiveTypes_[size_t(PrimitiveType::Char)] = Type::NewPrimitive(PrimitiveType::Char)) == nullptr)
    return false;
  if ((primitiveTypes_[size_t(PrimitiveType::Bool)] = Type::NewPrimitive(PrimitiveType::Bool)) == nullptr)
    return false;

  for (size_t i = 0; i < kTotalPrimitiveTypes; i++) {
    Type *type = primitiveTypes_[i];
    if ((referenceTypes_[i] = ReferenceType::New(type)) == nullptr)
      return false;
  }

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

Type *
TypeManager::newEnum(Atom *name)
{
  return EnumType::New(name);
}

