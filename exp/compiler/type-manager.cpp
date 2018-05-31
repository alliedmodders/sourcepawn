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

TypeManager::TypeManager(StringPool& strings)
 : strings_(strings),
   voidType_(nullptr),
   implicitVoidType_(nullptr),
   uncheckedType_(nullptr),
   metaFunctionType_(nullptr),
   overloadedFunctionType_(nullptr),
   primitiveTypes_(),
   char_type_(nullptr),
   char_array_(nullptr),
   const_char_array_(nullptr),
   float_type_(nullptr),
   float3_array_(nullptr),
   const_float3_array_(nullptr),
   variadic_any_(nullptr)
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
  implicitVoidType_ = Type::NewImplicitVoid();
  uncheckedType_ = Type::NewUnchecked();
  metaFunctionType_ = Type::NewMetaFunction();
  overloadedFunctionType_ = Type::NewOverloadedFunction();

  primitiveTypes_[size_t(PrimitiveType::ImplicitIntDoNotUseDirectly)] =
    Type::NewPrimitive(PrimitiveType::ImplicitIntDoNotUseDirectly);
#if 0
  primitiveTypes_[size_t(PrimitiveType::Int8)] = Type::NewPrimitive(PrimitiveType::Int8);
  primitiveTypes_[size_t(PrimitiveType::Uint8)] = Type::NewPrimitive(PrimitiveType::Uint8);
  primitiveTypes_[size_t(PrimitiveType::Int16)] = Type::NewPrimitive(PrimitiveType::Int16);
  primitiveTypes_[size_t(PrimitiveType::Uint16)] = Type::NewPrimitive(PrimitiveType::Uint16);
  primitiveTypes_[size_t(PrimitiveType::Uint32)] = Type::NewPrimitive(PrimitiveType::Uint32);
  primitiveTypes_[size_t(PrimitiveType::Int64)] = Type::NewPrimitive(PrimitiveType::Int64);
  primitiveTypes_[size_t(PrimitiveType::Uint64)] = Type::NewPrimitive(PrimitiveType::Uint64);
  primitiveTypes_[size_t(PrimitiveType::NativeInt)] = Type::NewPrimitive(PrimitiveType::NativeInt);
  primitiveTypes_[size_t(PrimitiveType::NativeUint)] = Type::NewPrimitive(PrimitiveType::NativeUint);
  primitiveTypes_[size_t(PrimitiveType::Double)] = Type::NewPrimitive(PrimitiveType::Double);
#endif

  primitiveTypes_[size_t(PrimitiveType::Int32)] = Type::NewPrimitive(PrimitiveType::Int32);
  primitiveTypes_[size_t(PrimitiveType::Float)] = Type::NewPrimitive(PrimitiveType::Float);
  primitiveTypes_[size_t(PrimitiveType::Char)] = Type::NewPrimitive(PrimitiveType::Char);
  primitiveTypes_[size_t(PrimitiveType::Bool)] = Type::NewPrimitive(PrimitiveType::Bool);

  // We special case the following types, because they are extremely common:
  //   char[]
  //   const char[]
  //   float[3]
  //   const float[3]
  char_type_ = getPrimitive(PrimitiveType::Char);
  char_array_ = ArrayType::New(char_type_, ArrayType::kUnsized);
  const_char_array_ =
    ArrayType::New(Type::NewQualified(char_type_, Qualifiers::Const),
                   ArrayType::kUnsized);

  float_type_ = getPrimitive(PrimitiveType::Float);
  float3_array_ = ArrayType::New(float_type_, 3);
  const_float3_array_ =
    ArrayType::New(Type::NewQualified(float_type_, Qualifiers::Const),
                   3);

  variadic_any_ = VariadicType::New(uncheckedType_);

  if (!reftype_cache_.init(16))
    return false;

  return true;
}

ArrayType*
TypeManager::newArray(Type* contained, int elements)
{
  if (contained == char_type_) {
    if (elements == ArrayType::kUnsized)
      return char_array_;
  } else if (contained == float_type_) {
    if (elements == 3)
      return float3_array_;
  }

  return ArrayType::New(contained, elements);
}

EnumType*
TypeManager::newEnum(Atom* name)
{
  return EnumType::New(name);
}

Type*
TypeManager::newQualified(Type* type, Qualifiers qualifiers)
{
  if (qualifiers == Qualifiers::Const) {
    if (type == char_array_)
      return const_char_array_;
    if (type == float3_array_)
      return const_float3_array_;
  }

  if ((type->qualifiers() & qualifiers) == qualifiers)
    return type;

  if (type->isQualified()) {
    qualifiers |= type->qualifiers();
    type = type->unqualified();
  }

  return Type::NewQualified(type, qualifiers);
}

TypesetType*
TypeManager::newTypeset(Atom* name)
{
  return TypesetType::New(name);
}

StructType*
TypeManager::newStruct(ast::RecordDecl* decl)
{
  return StructType::New(decl);
}

TypedefType*
TypeManager::newTypedef(Atom* name)
{
  return TypedefType::New(name);
}

ReferenceType*
TypeManager::newReference(Type* type)
{
  RefTypeCache::Insert p = reftype_cache_.findForAdd(type);
  if (p.found())
    return p->value;

  ReferenceType* ref = ReferenceType::New(type);
  if (!reftype_cache_.add(p, ref))
    return nullptr;
  return ref;
}

FunctionType*
TypeManager::newFunction(ast::FunctionSignature* sig)
{
  return FunctionType::New(sig);
}

VariadicType*
TypeManager::newVariadic(Type* inner)
{
  if (inner == uncheckedType_)
    return variadic_any_;
  return VariadicType::New(inner);
}

Type*
TypeManager::typeForLabelAtom(Atom* atom)
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
