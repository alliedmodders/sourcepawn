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
#ifndef _include_spcomp_boxed_value_h_
#define _include_spcomp_boxed_value_h_

#include "types.h"
#include "int-value.h"
#include "float-value.h"

namespace sp {

class BoxedValue
{
 public:
  enum class Kind {
    Opaque,
    Enum,
    Bool,
    Integer,
    Float
  };

 public:
  BoxedValue() {
    setOpaqueIntptr(0);
  }
  explicit BoxedValue(bool b)
   : kind_(Kind::Bool),
     type_(nullptr)
  {
    boolValue = b;
  }
  explicit BoxedValue(const IntValue& v)
   : kind_(Kind::Integer),
     type_(nullptr)
  {
    intValue = v;
  }
  explicit BoxedValue(const FloatValue& v)
   : kind_(Kind::Float),
     type_(nullptr)
  {
    floatValue = v;
  }
  BoxedValue(Type* type, const IntValue& v)
   : kind_(Kind::Integer),
     type_(type)
  {
    intValue = v;
  }

  Kind kind() const {
    return kind_;
  }

  bool isBool() const {
    return kind_ == Kind::Bool;
  }
  bool toBool() const {
    assert(isBool());
    return boolValue;
  }

  // Enums can attach an extra type.
  bool hasType() const {
    return !!type_;
  }
  Type* type() const {
    assert(hasType());
    return type_;
  }

  bool isInteger() const {
    return kind_ == Kind::Integer;
  }
  const IntValue& toInteger() const {
    assert(isInteger());
    return intValue;
  }

  bool isFloat() const {
    return kind_ == Kind::Float;
  }
  const FloatValue& toFloat() const {
    assert(isFloat());
    return floatValue;
  }

  bool isOpaque() const {
    return kind_ == Kind::Opaque;
  }
  void setOpaqueIntptr(intptr_t value) {
    kind_ = Kind::Opaque;
    intptrValue = value;
  }
  intptr_t toOpaqueIntptr() const {
    assert(isOpaque());
    return intptrValue;
  }

  const char* getTypename() const {
    switch (kind_) {
      case Kind::Enum:
        return "enum";
      case Kind::Bool:
        return "bool";
      case Kind::Integer:
        return intValue.getTypename();
      case Kind::Float:
        return floatValue.getTypename();
      default:
        assert(false);
        return "unknown";
    }
  }

 private:
  Kind kind_;
  Type* type_;
  union {
    bool boolValue;
    char charValue;
    IntValue intValue;
    FloatValue floatValue;
    double doubleValue;
    intptr_t intptrValue;
  };
};

BoxedValue DefaultValueForPlainType(Type* type);

}

#endif // _include_spcomp_boxed_value_h_
