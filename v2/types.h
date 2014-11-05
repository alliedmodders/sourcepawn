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

enum class PrimitiveType : uint32_t
{
  // A boolean type semantically represents 0 or 1. As an optimization, its
  // internal storage type is the same as int32, and any non-zero value
  // compares positively to "true".
  Bool,

  // A character is an arbitrary 8-bit value. Characters are not supported
  // as a normal storage class; they may only be used for arrays. When read
  // from an array, they are immediately sign extended to 32 bits. Integers
  // stored into an char array are truncated to 8 bits.
  Char,     // signed, 8-bit character

  // Enumerations and untyped variables are stored as signed, 32-bit integers.
  Int32,

  // Floating point values are 32-bit IEEE-754 floats. This is required ro
  // SourcePawn 1 compatibility.
  Float,

  TOTAL
};

static const size_t kTotalPrimitiveTypes = size_t(PrimitiveType::TOTAL);

enum class Qualifiers : uint32_t
{
  // None (the default).
  None      = 0x0,

  // Storage and mutability is constant.
  Const     = 0x1,

  // Storage is constant.
  ReadOnly  = 0x2
};
KE_DEFINE_ENUM_OPERATORS(Qualifiers);

class Type;
class TypedefType;
typedef FixedPoolList<Type *> TypeList;

#define TYPE_ENUM_MAP(_)        \
  _(Enum)                       \
  _(Reference)                  \
  _(Array)                      \
  _(Function)                   \
  _(Union)

#define FORWARD_DECLARE(name) class name##Type;
TYPE_ENUM_MAP(FORWARD_DECLARE)
#undef FORWARD_DECLARE

class Type : public PoolObject
{
 protected:
  enum class Kind {
    // A primitive is a plain-old-data type.
    Primitive,

    // Void is an internal type used to specify that a function returns no
    // value.
    Void,

    // Enums are plain-old-data with int32 storage. They allow for extra
    // type checking that would not be possible with integers.
    Enum,

    // A named typedef.
    Typedef,

    // Qualifying wrapper.
    Qualifier,

    // Unchecked is a magic type that has implicit, bitwise coercion to
    // int32, float, bool, or an enum.
    Unchecked,

    // An array is a fixed-length vector of any other type.
    Array,

    // A discriminated union of types.
    Union,

    // A value-typed composite type.
    Struct,

    // A reference type may only be specified on parameters, and
    // references may only be computed to primitives or enums.
    Reference,

    // A function type encapsulates a function signature.
    Function,

    // The type used for the "Function" tag.
    MetaFunction
  };

  void init(Kind kind, Type *root = NULL);

 protected:
  Type(Kind kind)
   : kind_(kind)
  {
    canonical_ = this;
  }
  Type(Kind kind, Type *canonical)
   : kind_(kind),
     canonical_(canonical)
  {
  }

 public:   
  static Type *NewVoid();
  static Type *NewMetaFunction();
  static Type *NewUnchecked();
  static Type *NewPrimitive(PrimitiveType type);
  static Type *NewQualified(Type *type, Qualifiers quals);
  static Type *NewImportable();
  static bool Compare(Type *left, Type *right);

  bool isPrimitive() {
    return canonical()->kind_ == Kind::Primitive;
  }
  bool isVoid() {
    return canonical()->kind_ == Kind::Void;
  }
  bool isUnchecked() {
    return canonical()->kind_ == Kind::Unchecked;
  }
  bool isMetaFunction() {
    return canonical()->kind_ == Kind::MetaFunction;
  }

  // Note that isTypedef() is special in that it does not bypass wrappers.
  bool isTypedef() {
    return kind_ == Kind::Typedef;
  }
  bool isUnresolvedTypedef() {
    return kind_ == Kind::Typedef && canonical_ == this;
  }
  bool isResolvedTypedef() {
    return kind_ == Kind::Typedef && canonical_ != this;
  }
  TypedefType *toTypedef() {
    assert(isTypedef());
    return (TypedefType *)this;
  }

  bool canUseInReferenceType() {
    return isPod();
  }

  PrimitiveType primitive() {
    assert(isPrimitive());
    return canonical()->primitive_;
  }

  bool isPod() {
    return isPrimitive() || isEnum() || isUnchecked();
  }
  PrimitiveType pod() {
    assert(isPod());
    return isPrimitive()
           ? (primitive() == PrimitiveType::Bool)
             ? PrimitiveType::Int32
             : primitive()
           : PrimitiveType::Int32;
  }
  bool isChar() {
    return isPrimitive() && primitive() == PrimitiveType::Char;
  }
  bool isFloat() {
    return isPrimitive() && primitive() == PrimitiveType::Float;
  }
  bool isInt32() {
    return isPrimitive() && primitive() == PrimitiveType::Int32;
  }
  bool isInt32OrEnum() {
    return isInt32() || isEnum();
  }
  bool isBool() {
    return isPrimitive() && primitive() == PrimitiveType::Bool;
  }

  Qualifiers qualifiers() {
    Type *norm = normalized();
    if (norm->kind_ != Kind::Qualifier)
      return Qualifiers::None;
    return norm->qualifiers_;
  }
  bool isQualified() {
    return normalized()->kind_ == Kind::Qualifier;
  }
  Type *unqualified() {
    return canonical();
  }

#define CAST(name)                                \
  bool is##name() {                               \
    return canonical()->kind_ == Kind::name;      \
  }                                               \
  name##Type *to##name() {                        \
    assert(is##name());                           \
    return (name##Type *)canonical();             \
  }                                               \
  name##Type *as##name() {                        \
    if (!is##name())                              \
      return nullptr;                             \
    return to##name();                            \
  }
  TYPE_ENUM_MAP(CAST)
#undef CAST

 protected:
  bool isWrapped() const {
    return canonical_ != this;
  }

 public:
  // Return the underlying canonical type, meaning, the current type
  // normalized and without its wrappings.
  Type *canonical() {
    // Keep canonical bits up to date.
    if (isWrapped() && canonical_->isResolvedTypedef())
      normalize();
    return canonical_;
  }

 protected:
  // Desugar typedefs until we reach a valid type or a typedef that has not
  // been resolved.
  Type *normalized() {
    if (isTypedef()) {
      if (canonical_->isResolvedTypedef())
        normalize();

      // We might still return a typedef here, for example if we are unresovled
      // or we're wrapping something unresolved.
      if (isTypedef())
        return canonical_;
    }

    // We're either an actual type or a qualifier.
    return this;
  }

  // Collapse redundant pairs of wrappers.
  //   (qual -> qual) -> (qual)
  //   (typedef -> qual) -> (qual)
  //   (qual -> typedef) -> (qual)
  //
  // If a typedef chain ends in an unresolved typedef, then
  // normalization stops.
  void normalize() {
    assert(isWrapped() && canonical_->isResolvedTypedef());
    do {
      canonical_ = canonical_->normalized();

      // We ignore quals -> placholder.
      if (canonical_->kind_ != Kind::Qualifier)
        break;

      assert(kind_ == Kind::Qualifier || kind_ == Kind::Typedef);

      // Assume the qualifiers of our inner type, then reach behind the quals.
      // This could cause us to hit another typedef, so we have to be prepared
      // to collapse again. This could happen with something really nasty like:
      //     typedef E const D
      //     typedef D const C
      //     typedef C const B
      // etc...
      kind_ = Kind::Qualifier;
      qualifiers_ |= canonical_->qualifiers_;
      canonical_ = canonical_->canonical_;
    } while (isWrapped() && canonical_->isResolvedTypedef());
  }

 protected:
  Kind kind_;
  union {
    PrimitiveType primitive_;
    Qualifiers qualifiers_;
    uint32_t flags_;
  };

  // For unqualified types, this points to |this|. For qualified types, it
  // points to the unqualified original type.
  //
  // For typedefs, it points to |this| until the actual canonical is known.
  Type *canonical_;
};

class EnumType : public Type
{
  EnumType()
   : Type(Kind::Enum)
  {
    flags_ = 0;
  }

  static const uint32_t kCreatedForMethodmap = 0x1;
  static const uint32_t kHasMethodmap = 0x2;

 public:
  static EnumType *New(Atom *name);

  Atom *name() {
    return name_;
  }

  void setCreatedForMethodmap() {
    flags_ |= kCreatedForMethodmap|kHasMethodmap;
  }
  void unsetCreatedForMethodmap() {
    flags_ &= ~kCreatedForMethodmap;
  }
  bool wasCreatedForMethodmap() {
    return !!(flags_ & kCreatedForMethodmap);
  }
  void setHasMethodmap() {
    flags_ |= kHasMethodmap;
  }
  bool hasMethodmap() {
    return !!(flags_ & kHasMethodmap);
  }

 private:
  Atom *name_;
};

class ReferenceType : public Type
{
  ReferenceType()
   : Type(Kind::Reference)
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
  static const int kUnsized = 0;

  static ArrayType *New(Type *contained, int elements);

  Type *innermost() const {
    Type *temp = contained();
    while (temp->isArray())
      temp = temp->toArray()->contained();
    return temp;
  }

  Type *contained() const {
    return contained_;
  }
  bool isCharArray() const {
    return contained()->isPrimitive() &&
           contained()->primitive() == PrimitiveType::Char;
  }
  int32_t size() const {
    return elements_;
  }

 private:
  int32_t elements_;
  Type *contained_;
};

class TypedefType : public Type
{
  TypedefType(Atom *name)
   : Type(Kind::Typedef),
     name_(name)
  {}

 public:
  static TypedefType *New(Atom *name, Type *actual = nullptr);

  void resolve(Type *actual) {
    canonical_ = actual;
    actual_ = actual;
  }
  Atom *name() const {
    return name_;
  }
  Type *actual() const {
    return actual_;
  }

 private:
  Atom *name_;
  Type *actual_;
};

class FunctionSignature;

class FunctionType : public Type
{
  FunctionType(FunctionSignature *signature)
   : Type(Kind::Function),
     signature_(signature)
  {
  }

 public:
  static FunctionType *New(FunctionSignature *sig);

  FunctionSignature *signature() const {
    return signature_;
  }

 private:
  Atom *name_;
  FunctionSignature *signature_;
};

class UnionType : public Type
{
  UnionType(Atom *atom)
   : Type(Kind::Union)
  {}

 public:
  static UnionType *New(Atom *atom);

  Atom *name() const {
    return name_;
  }

  void setTypes(TypeList *types) {
    types_ = types;
  }
  TypeList *types() const {
    return types_;
  }

 private:
  Atom *name_;
  TypeList *types_;
};

class StructType : public Type
{
  StructType(Atom *atom)
    : Type(Kind::Struct)
  {}

 public:
  static StructType *New(Atom *atom);

  Atom *name() const {
    return name_;
  }

 private:
  Atom *name_;
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
    b.type_ = PrimitiveType::Int32;
    b.i_ = i;
    return b;
  }
  static BoxedPrimitive Float(float f) {
    BoxedPrimitive b;
    b.type_ = PrimitiveType::Float;
    b.f_ = f;
    return b;
  }
  static BoxedPrimitive Bool(bool b) {
    BoxedPrimitive box;
    box.type_ = PrimitiveType::Bool;
    box.b_ = b;
    return box;
  }

  PrimitiveType type() const {
    return type_;
  }
  bool isBool() const {
    return type() == PrimitiveType::Bool;
  }
  bool isInt() const {
    return type() == PrimitiveType::Int32;
  }
  bool isFloat() const {
    return type() == PrimitiveType::Float;
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

const char *GetPrimitiveName(PrimitiveType type);
const char *GetTypeName(Type *type);
const char *GetTypeClassName(Type *type);

}

#endif // _include_jc_types_h_
