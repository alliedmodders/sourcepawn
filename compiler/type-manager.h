/* vim: set ts=4 sw=4 tw=99 et:
 *
 * Copyright (C) 2012 David Anderson
 *
 * This file is part of SourcePawn.
 *
 * SourcePawn is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 * 
 * SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * SourcePawn. If not, see http://www.gnu.org/licenses/.
 */
#ifndef _include_jitcraft_type_manager_h_
#define _include_jitcraft_type_manager_h_

#include "string-pool.h"
#include "types.h"

namespace sp {

class String;

class TypeManager
{
 public:
  TypeManager(StringPool &pool);

  bool initialize();

  Type *getPrimitive(PrimitiveType type) {
    return primitiveTypes_[size_t(type)];
  }

  Type *getVoid() {
    return voidType_;
  }
  Type *getImplicitVoid() {
    return implicitVoidType_;
  }
  Type *getUnchecked() {
    return uncheckedType_;
  }
  Type *getMetaFunction() {
    return metaFunctionType_;
  }
  Type *getNullType() const {
    return nullType_;
  }
  ReferenceType *newReference(Type *type);
  ArrayType *newArray(Type *contained, int elements);
  EnumType *newEnum(Atom *name);
  Type *newQualified(Type *type, Qualifiers qualifiers);
  TypesetType *newTypeset(TypesetDecl *decl);
  StructType *newStruct(RecordDecl *decl);
  TypedefType *newTypedef(Atom *name);

  Type *typeForLabelAtom(Atom *atom);

 private:
  StringPool &strings_;
  Type *voidType_;
  Type *implicitVoidType_;
  Type *uncheckedType_;
  Type *metaFunctionType_;
  Type *nullType_;
  Type *primitiveTypes_[kTotalPrimitiveTypes];
  ReferenceType *referenceTypes_[kTotalPrimitiveTypes];

  Type *char_type_;
  ArrayType *char_array_;
  Type *const_char_array_;

  Type *float_type_;
  ArrayType *float3_array_;
  Type *const_float3_array_;

  Atom *atom_String_;
  Atom *atom_Float_;
  Atom *atom_any_;
  Atom *atom_Function_;
  Atom *atom_bool_;
};

}

#endif // _include_jitcraft_type_manager_h_
