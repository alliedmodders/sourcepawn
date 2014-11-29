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

namespace sp {

struct ReportingContext;
class TypeSpecifier;

enum class PrimitiveType : uint32_t
{
  // A boolean type semantically represents 0 or 1. As an optimization, its
  // internal storage type is the same as int32, and any non-zero value
  // compares positively to "true".
  Bool,

  // A char is an 8-bit signed integer that has some extra coercion rules for
  // legacy code.
  Char,

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
  _(Union)                      \
  _(Struct)

#define FORWARD_DECLARE(name) class name##Type;
TYPE_ENUM_MAP(FORWARD_DECLARE)
#undef FORWARD_DECLARE
class RecordType;

class Type : public PoolObject
{
 public:
  enum class Kind {
    // A type that could not be resolved.
    Unresolvable,

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

 public:
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
  bool isUnresolvable() {
    return canonical()->kind_ == Kind::Unresolvable;
  }

  // Note that isTypedef() is special in that it does not bypass wrappers.
  bool isResolvedTypedef() const {
    return kind_ == Kind::Typedef && canonical_ != this;
  }
  bool isUnresolvedTypedef() const {
    return kind_ == Kind::Typedef && canonical_ == this;
  }
  TypedefType *toTypedef() {
    assert(kind_ == Kind::Typedef);
    return (TypedefType *)this;
  }
  TypedefType *asTypedef() {
    if (kind_ != Kind::Typedef)
      return nullptr;
    return (TypedefType *)this;
  }

  bool canUseInReferenceType() {
    return isPod();
  }
  bool canBeUsedInConstExpr() {
    return isPrimitive() || isEnum();
  }

  PrimitiveType primitive() {
    assert(isPrimitive());
    return canonical()->primitive_;
  }

  bool isConst() {
    return (qualifiers() & Qualifiers::Const) == Qualifiers::Const;
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

  // Check whether this type pointer is actually resolved to anything. This
  // will return true for Unresolvable types, since technically it has been
  // resolved to an error. It will return false if the canonical type is a
  // typedef that has not been resolved.
  bool isResolved() {
    // Canonical type should only be a typedef if it is unresolved.
    assert(canonical()->kind_ != Kind::Typedef ||
           canonical()->isUnresolvedTypedef());
    return canonical()->isUnresolvedTypedef();
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

  // Record type covers multiple types, so we have separate accessors here.
  bool isRecord() {
    return isUnion() || isStruct();
  }
  RecordType *toRecord() {
    assert(isRecord());
    return (RecordType *)this;
  }
  RecordType *asRecord() {
    if (!isRecord())
      return nullptr;
    return toRecord();
  }

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
    if (isResolvedTypedef()) {
      if (canonical_->isResolvedTypedef())
        normalize();

      // We might still return a typedef here, for example if we are unresovled
      // or we're wrapping something unresolved.
      if (isWrapped())
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

class Methodmap;

class EnumType : public Type
{
  EnumType()
   : Type(Kind::Enum),
     methodmap_(nullptr)
  {
  }

 public:
  static EnumType *New(Atom *name);

  Atom *name() {
    return name_;
  }

  void setMethodmap(Methodmap *methodmap) {
    assert(!methodmap_);
    methodmap_ = methodmap;
  }
  Methodmap *methodmap() const {
    return methodmap_;
  }

 private:
  Atom *name_;
  Methodmap *methodmap_;
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
  // This array does not have a size specified. It may or may not internally
  // have a fixed size.
  static const int kUnsized = -1;

  // This array's size is indeterminate. This is different from Unsized; an
  // indeterminate rank implies that the number of elements is fixed, but
  // not known. For example:
  //   int a[][] = {
  //      {1, 2},
  //      {3, 4, 5},
  //   };
  //
  // This is an ArrayType(ArrayType(Primitive::Int32, Indeterminate), 2). The
  // initial rank is known to have a size of 2, but the next rank's size is not
  // known. The distinction is for SourcePawn 1 sizeof() and assignment
  // compatibility.
  static const int kIndeterminate = -2;

  // Maximum size of an array. We choose this value because we can compute
  // addresses as multiples of an index without overflowing.
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
  //:TODO: remove this.
  bool isCharArray() const {
    return contained()->isPrimitive() &&
           contained()->primitive() == PrimitiveType::Char;
  }
  bool hasFixedSize() const {
    return elements_ >= 0;
  }
  int32_t fixedSize() const {
    assert(hasFixedSize());
    return elements_;
  }

  bool equalTo(ArrayType *other) {
    return elements_ == other->elements_ &&
           contained_ == other->contained_;
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
  static TypedefType *New(Atom *name);

  Atom *name() const {
    return name_;
  }

  // The actual type may be null if it is unresolved. It is only guaranteed to
  // be set if the TypeResolver phase passes.
  Type *actual() const {
    return actual_;
  }
  void resolve(Type *actual);

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
  FunctionSignature *signature_;
};

class RecordType : public Type
{
 public:
  RecordType(Kind kind, Atom *name)
   : Type(kind),
     name_(name)
  {}

  Atom *name() const {
    return name_;
  }

 private:
  Atom *name_;
};

class UnionType : public RecordType
{
  UnionType(Atom *atom)
   : RecordType(Kind::Union, atom)
  {}

 public:
  static UnionType *New(Atom *atom);

  void setTypes(TypeList *types) {
    types_ = types;
  }
  TypeList *types() const {
    return types_;
  }

 private:
  TypeList *types_;
};

class StructType : public RecordType
{
  StructType(Atom *atom)
   : RecordType(Kind::Struct, atom)
  {}

 public:
  static StructType *New(Atom *atom);
};

// This should probably be in the type manager... but it should never leak past
// type resolution.
extern Type UnresolvableType;

const char *GetPrimitiveName(PrimitiveType type);
const char *GetTypeName(Type *type);
const char *GetTypeClassName(Type *type);
AString BuildTypeName(Type *type);

// Similar to BuildTypeName, but an optional atom can be specified to correctly
// insert a variable name in pre- or post- order.
AString BuildTypeName(const TypeSpecifier *spec, Atom *name);

// Compute the size of a type. It must be an array type, and it must have
// at least as many levels as specified, and the specified level must be
// determinate (fixed).
//
// On failure, returns 0 and an error will have been reported.
int32_t ComputeSizeOfType(ReportingContext &cc, Type *type, size_t level);

}

#endif // _include_jc_types_h_
