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

#include "types.h"

namespace ke {

class String;

class TypeManager
{
  // Primitive type cache.
  Type *voidType_;
  Type *uncheckedType_;
  Type *primitiveTypes_[kTotalPrimitiveTypes];
  ReferenceType *referenceTypes_[kTotalPrimitiveTypes];

 public:
  TypeManager();

  bool initialize();

  Type *getPrimitive(PrimitiveType type) {
    return primitiveTypes_[size_t(type)];
  }

  Type *getVoid() {
    return voidType_;
  }
  Type *getUnchecked() {
    return uncheckedType_;
  }
  ReferenceType *newReference(Type *type);
  ArrayType *newArray(Type *contained, int elements);
  Type *newEnum(Atom *name);
  Type *newQualified(Type *type, Qualifiers qualifiers);
};

}

#endif // _include_jitcraft_type_manager_h_
