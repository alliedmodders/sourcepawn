// vim: set sts=2 ts=8 sw=2 tw=99 et:
// 
// Copyright (C) 2012 David Anderson
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

#ifndef _include_jc_types_h_
#define _include_jc_types_h_

#include "assert.h"
#include "tokens.h"
#include "string-pool.h"
#include "pool-allocator.h"

namespace ke {

static const size_t MAX_ARRAY_DEPTH = 5;

enum PrimitiveType
{
  // A boolean type semantically represents 0 or 1. As an optimization, its
  // internal storage type is the same as int32, and any non-zero value
  // compares positively to "true".
  PrimitiveType_Bool,

  // A character is an arbitrary 8-bit value. Characters are not supported
  // as a normal storage class; they may only be used for arrays. When read
  // from an array, they are immediately sign extended to 32 bits. Integers
  // stored into an char array are truncated to 8 bits.
  PrimitiveType_Char,     // signed, 8-bit character

  // Enumerations and untyped variables are stored as signed, 32-bit integers.
  PrimitiveType_Int32,

  // Floating point values are 32-bit IEEE-754 floats. This is required ro
  // SourcePawn 1 compatibility.
  PrimitiveType_Float,

  // This is a native-sized integer type (32-bit on 32-bit systems, 64-bit
  // on 64-bit systems), and does not exist in language semantics.
  PrimitiveType_Native,

  PrimitiveTypes_Total
};

enum class TypeQuals : uint32_t
{
  // None (the default).
  None      = 0x0,

  // Storage and mutability is constant.
  Const     = 0x1,

  // Value of expression is known at compile-time.
  ConstExpr = 0x2,

  // Function arguments only.
  Variadic  = 0x4
};

static inline TypeQuals operator &(const TypeQuals &x, const TypeQuals &y) {
  return TypeQuals(uint32_t(x) & uint32_t(y));
}
static inline TypeQuals operator |(const TypeQuals &x, const TypeQuals &y) {
  return TypeQuals(uint32_t(x) | uint32_t(y));
}
static inline TypeQuals& operator |=(TypeQuals &x, const TypeQuals &y) {
  x = x | y;
  return x;
}
static inline bool operator !(const TypeQuals &x) {
  return !uint32_t(x);
}

class Type;
typedef FixedPoolList<Type *> TypeList;

#define TYPE_ENUM_MAP(_)  \
  _(Enum)                 \
  _(Reference)            \
  _(Array)                \
  _(Function)

#define FORWARD_DECLARE(name) class name##Type;
TYPE_ENUM_MAP(FORWARD_DECLARE)
#undef FORWARD_DECLARE

class Type : public PoolObject
{
 protected:
  enum Kind {
    // A primitive is a plain-old-data type.
    PRIMITIVE,

    // Void is an internal type used to specify that a function returns no
    // value.
    VOID,

    // Enums are plain-old-data with int32 storage. They allow for extra
    // type checking that would not be possible with integers.
    ENUM,

    // Unchecked is a magic type that has implicit, bitwise coercion to
    // int32, float, bool, or an enum.
    UNCHECKED,

    // Contains type qualifiers, and a back pointer to the actual
    // unqualified type.
    QUALTYPE,

    // ------ Add traceable types below (STRING FIRST!) -------

    // An array is a fixed-length vector of any other type.
    ARRAY,
    EXTERNAL_ARRAY,

    // A reference type may only be specified on parameters, and
    // references may only be computed to primitives or enums.
    REFERENCE,

    // A function type encapsulates a function signature.
    FUNCTION,

    // The type of types.
    TYPE
  };

  void init(Kind kind, Type *root = NULL);

 protected:
  Type(Kind kind)
   : kind_(kind),
     root_(this)
  {
  }

  Kind kind() {
    return root_->kind_;
  }

 public:   
  static Type *NewVoid();
  static Type *NewUnchecked();
  static Type *NewType();
  static Type *NewPrimitive(PrimitiveType type);
  static Type *NewQualified(TypeQuals qual, Type *type);
  static Type *NewImportable();
  static bool Compare(Type *left, Type *right);

  bool isPrimitive() {
    return kind() == PRIMITIVE;
  }
  bool isArray() {
    return kind() == ARRAY || kind() == EXTERNAL_ARRAY;
  }
  bool isReference() {
    return kind() == REFERENCE;
  }
  bool isFunction() {
    return kind() == FUNCTION;
  }
  bool isVoid() {
    return kind() == VOID;
  }
  bool isUnchecked() {
    return kind() == UNCHECKED;
  }

  // Structs and fixed-length arrays are always allocated inline, but they are
  // always passed by-reference. It is impossible to compute a reference to
  // the location holding their address, because no such location exists. This
  // function returns true for storage loations of these types where such
  // explicit references cannot be computed.
  //
  // Note that this only refers to the type; an array of this type existing in
  // an argument, currently, has the same type - so the storage location must
  // be checked as well.
  inline bool addressIsInline();

  PrimitiveType primitive() {
    assert(isPrimitive());
    return root_->primitive_;
  }
  bool isPod() {
    return isPrimitive() || isEnum() || isUnchecked();
  }
  PrimitiveType pod() {
    assert(isPod());
    return isPrimitive()
           ? (primitive() == PrimitiveType_Bool)
             ? PrimitiveType_Int32
             : primitive()
           : PrimitiveType_Int32;
  }
  bool isChar() {
    return isPrimitive() && primitive() == PrimitiveType_Char;
  }
  bool isFloat() {
    return isPrimitive() && primitive() == PrimitiveType_Float;
  }
  bool isInt32() {
    return isPrimitive() && primitive() == PrimitiveType_Int32;
  }
  bool isInt32OrEnum() {
    return isInt32() || isEnum();
  }
  bool isBool() {
    return isPrimitive() && primitive() == PrimitiveType_Bool;
  }
  bool isExternalArray() {
    return kind() == EXTERNAL_ARRAY;
  }
  bool isEnum() {
    return kind() == ENUM;
  }
  bool isType() {
    return kind() == TYPE;
  }
  bool hasTypeQuals() {
    return kind_ == QUALTYPE;
  }

  TypeQuals quals() {
    if (!hasTypeQuals())
      return TypeQuals::None;
    return quals_;
  }
  Type *unqualified() {
    return root_;
  }

#define CAST(name)                                \
  name##Type *to##name() {                        \
    assert(is##name());                           \
    return (name##Type *)this;                    \
  }                                               \
  name##Type *as##name() {                        \
    if (!is##name())                              \
      return nullptr;                             \
    return to##name();                            \
  }
  TYPE_ENUM_MAP(CAST)
#undef CAST

 protected:
  Kind kind_;
  PrimitiveType primitive_;
  TypeQuals quals_;

  // For unqualified types, this points to |this|. For qualified types, it
  // points to the unqualified original type.
  Type *root_;
};

class EnumType : public Type
{
  EnumType()
   : Type(ENUM)
  {
  }

 public:
  static EnumType *New(Atom *name);

  Atom *name() {
    return name_;
  }

 private:
  Atom *name_;
};

class ReferenceType : public Type
{
  ReferenceType()
   : Type(REFERENCE)
  {
  }

 public:
  static ReferenceType *New(Type *contained);
  
  Type *contained() {
    return contained_;
  }

 private:
  Type *contained_;
};

class ArrayType : public Type
{
  ArrayType(Kind kind)
   : Type(kind)
  {
  }

 public:
  static const int DYNAMIC_ARRAY_SIZE = -1;

  static ArrayType *New(Type *contained, int elements);
  static ArrayType *NewExternal(Type *contained);

  Type *innermost() {
    Type *temp = contained();
    while (temp->isArray())
      temp = temp->toArray()->contained();
    return temp;
  }

  bool isDynamic() {
    return elements_ == DYNAMIC_ARRAY_SIZE;
  }
  bool isExtensible() {
    return isDynamic() && !(quals_ & TypeQuals::Const);
  }
  Type *contained() {
    return contained_;
  }
  bool isFixedLength() {
    return elements_ > 0;
  }
  bool isCharArray() {
    return contained()->isPrimitive() &&
         contained()->primitive() == PrimitiveType_Char;
  }
  unsigned fixedLength() {
    assert(isFixedLength());
    return elements_;
  }
  unsigned levels() {
    return levels_;
  }

 private:
  int32_t elements_;
  unsigned levels_;
  Type *contained_;
};

class FunctionType : public Type
{
  FunctionType()
   : Type(FUNCTION)
  {
  }

 public:
  static FunctionType *New(TokenKind kind);

  Type *parameterAt(size_t i) {
    return parameters()->at(i);
  }
  Type *returnType() {
    assert(isFunction());
    return returnType_;
  }
  TypeList *parameters() {
    assert(isFunction());
    return parameters_;
  }
  // FixedArray *defaults() {
  //   assert(isFunction());
  //   return defaults_;
  // }
  bool isNative() {
    return token_ == TOK_NATIVE;
  }
  bool isForward() {
    return token_ == TOK_FORWARD;
  }
  bool isNativeVariadic() {
    assert(isNative());
    return variadicNative_;
  }
  void setVariadic() {
    assert(isNative());
    variadicNative_ = true;
  }
  void setReturnType(Type *returnType) {
    assert(!returnType_);
    returnType_ = returnType;
  }
  void setParameterTypes(TypeList *types) {
    assert(!parameters_);
    parameters_ = types;
  }
  TokenKind token() {
    return token_;
  }
  Atom *name() {
    return name_;
  }

 private:
  // Functions only.
  Type *returnType_;
  TypeList *parameters_;
  // FixedArray *defaults_;
  Atom *name_;
  TokenKind token_;
  bool variadicNative_;
};

class BoxedPrimitive
{
  PrimitiveType type_;
  union {
    bool b_;
    int i_;
    float f_;
  };

 public:
  static BoxedPrimitive Int(int i) {
    BoxedPrimitive b;
    b.type_ = PrimitiveType_Int32;
    b.i_ = i;
    return b;
  }
  static BoxedPrimitive Float(float f) {
    BoxedPrimitive b;
    b.type_ = PrimitiveType_Float;
    b.f_ = f;
    return b;
  }
  static BoxedPrimitive Bool(bool b) {
    BoxedPrimitive box;
    box.type_ = PrimitiveType_Bool;
    box.b_ = b;
    return box;
  }

  PrimitiveType type() const {
    return type_;
  }
  bool isBool() const {
    return type() == PrimitiveType_Bool;
  }
  bool isInt() const {
    return type() == PrimitiveType_Int32;
  }
  bool isFloat() const {
    return type() == PrimitiveType_Float;
  }
  bool toBool() const {
    assert(isBool());
    return b_;
  }
  int toInt() const {
    assert(isInt());
    return i_;
  }
  float toFloat() const {
    assert(isFloat());
    return f_;
  }
};

inline bool
Type::addressIsInline()
{
  if (isArray())
    return toArray()->isFixedLength();
  return false;
}

const char *GetTypeName(Type *type);

}

#endif // _include_jc_types_h_
