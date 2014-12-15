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
#ifndef _include_spcomp_expression_ops_h_
#define _include_spcomp_expression_ops_h_

#include <am-utility.h>
#include "boxed-value.h"

namespace sp {

#if 0
class ExprOp;

// An s-value encodes the result of an expression as either an r-value or an
// l-value.
class SVal
{
 public:
  enum class Kind {
    Var,
    Func,
    Expr,
    Op,
    Const
  };

  Kind kind() const {
    return kind_;
  }
  Type *type() const {
    return type_;
  }
  VarDecl *var() const {
    assert(kind_ == Kind::Var);
    return var_;
  }
  FunctionStatement *fun() const {
    assert(kind_ == Kind::Func);
    return fun_;
  }
  Expression *expr() const {
    assert(kind_ == Kind::Expr);
    return expr_;
  }
  ExprOp *op() const {
    assert(kind_ == Kind::Op);
    return op_;
  }
  const BoxedValue &value() const {
    assert(kind_ == Kind::Const);
    return *boxval_.address();
  }

 private:
  Kind kind_;
  Type *type_;
  union {
    VarDecl *var_;
    FunctionStatement *fun_;
    Expression *expr_;
    ExprOp *op_;
    StorageBuffer<BoxedValue> boxval_;
  };
};
#endif

// SourcePawn requires value classes similar to C++.
enum class VK
{
  none,

  // An lvalue can be the left-hand side of an assignment. LValues in Pawn are:
  //   NameProxies
  //   FieldExprs
  //   IndexExprs
  lvalue,

  // A clvalue is an lvalue that is immutable. We do not compute reference
  // types for lvalues, so this is needed to distinguish between const and
  // non-const lvalues.
  clvalue,

  // An rvalue is anything that is not an lvalue.
  rvalue,

  // An xvalue is an "expiring" value, meaning that it will be destroyed after
  // the invoking statement concludes. Expiring values are created for
  // temporary references, for example:
  //     void f(int &x = 10);
  //
  //     f();
  //
  // In this example, the value produced for passing 10 into |x| will be an
  // xvalue. This only applies to default values.
  xvalue 
};

#define EXPROP_MAP(_)                     \
  _(Deref)                                \
  _(LvalToRef)                            \
  _(ToUnchecked)                          \
  _(ToBool)                               \
  _(UncheckedToTyped)                     \
  _(IntToFloat)                           \
  _(FloatToDouble)                        \
  _(NullCast)                             \
  _(IntegerCast)                          \
  _(ToTypeset)                            \
  _(FixedArrayDecay)

// Forward declarations.
#define _(name) class name##Op;
EXPROP_MAP(_)
#undef _

class ExprOpVisitor
{
 public:
#define _(name) virtual void visit##name##Op(name##Op *op) = 0;
EXPROP_MAP(_)
#undef _
};

// Expression Ops are attached to expressions in the AST. They are generated
// during semantic analysis to inform code generation of the right sequence
// of implicit conversions to take after evaluating an expression.
class ExprOp : public PoolObject
{
 public:
  ExprOp(ExprOp *prev, Type *type, VK vk)
   : prev_(prev),
     type_(type),
     valueKind_(vk)
  {}

  enum class Kind {
#define _(name) name,
    EXPROP_MAP(_)
#undef _
    sentinel
  };

  virtual Kind kind() const = 0;
  virtual void accept(ExprOpVisitor *cg) = 0;

  Type *type() const {
    return type_;
  }
  VK vk() const {
    return valueKind_;
  }
  ExprOp *prev() const {
    return prev_;
  }

#define _(name)                               \
  bool is##name##Op() const {                 \
    return kind() == Kind::name;              \
  }                                           \
  name##Op *as##name##Op() {                  \
    if (is##name##Op())                       \
      return nullptr;                         \
    return to##name##Op();                    \
  }                                           \
  name##Op *to##name##Op() {                  \
    assert(is##name##Op());                   \
    return (name##Op *)this;                  \
  }
  EXPROP_MAP(_)
#undef _

 private:
  ExprOp *prev_;
  Type *type_;
  VK valueKind_;
};

#define DECLARE_EXPROP(k)                     \
  Kind kind() const override {                \
    return Kind::k;                           \
  }                                           \
  void accept(ExprOpVisitor *cg) {            \
    cg->visit##k##Op(this);                   \
  }

// ref(T) -> T
class DerefOp : public ExprOp
{
 public:
  DerefOp(ExprOp *prev, Type *type)
   : ExprOp(prev, type, VK::rvalue)
  {}

  DECLARE_EXPROP(Deref);
};

// lval -> ref(lval)
class LvalToRefOp : public ExprOp
{
 public:
  LvalToRefOp(ExprOp *prev, Type *type)
   : ExprOp(prev, type, VK::lvalue)
  {
    assert(type->isReference());
  }

  DECLARE_EXPROP(LvalToRef);
};

// primitive|enum -> unchecked
class ToUncheckedOp : public ExprOp
{
 public:
  ToUncheckedOp(ExprOp *prev, Type *type)
   : ExprOp(prev, type, VK::rvalue)
  {}

  DECLARE_EXPROP(ToUnchecked);
};

// primitive|enum -> bool
class ToBoolOp : public ExprOp
{
 public:
  ToBoolOp(ExprOp *prev, Type *type)
   : ExprOp(prev, type, VK::rvalue)
  {} 

  DECLARE_EXPROP(ToBool);
};

// unchecked -> primitive|enum
class UncheckedToTypedOp : public ExprOp
{
 public:
  UncheckedToTypedOp(ExprOp *prev, Type *type)
   : ExprOp(prev, type, VK::rvalue)
  {
    assert(type->isUnchecked());
  }

  DECLARE_EXPROP(UncheckedToTyped);
};

// int* -> float
class IntToFloatOp : public ExprOp
{
 public:
  IntToFloatOp(ExprOp *prev, Type *type)
   : ExprOp(prev, type, VK::rvalue)
  {
    assert(type->primitive() == PrimitiveType::Float);
  }

  DECLARE_EXPROP(IntToFloat);
};

// float -> double
class FloatToDoubleOp : public ExprOp
{
 public:
  FloatToDoubleOp(ExprOp *prev, Type *type)
   : ExprOp(prev, type, VK::rvalue)
  {
    assert(type->primitive() == PrimitiveType::Double);
  }

  DECLARE_EXPROP(FloatToDouble);
};

// null_t -> enum(0)
class NullCastOp : public ExprOp
{
 public:
  NullCastOp(ExprOp *prev, Type *type)
   : ExprOp(prev, type, VK::rvalue)
  {
  }

  DECLARE_EXPROP(NullCast);
};

// intN -> intN
class IntegerCastOp : public ExprOp
{
 public:
  IntegerCastOp(ExprOp *prev, Type *type)
   : ExprOp(prev, type, VK::rvalue)
  {}

  DECLARE_EXPROP(IntegerCast);
};

// * -> typeset
class ToTypesetOp : public ExprOp
{
 public:
  ToTypesetOp(ExprOp *prev, size_t typeIndex, Type *to)
   : ExprOp(prev, to, VK::rvalue),
     type_index_(typeIndex)
  {}

  DECLARE_EXPROP(ToTypeset);

  size_t typeIndex() const {
    return type_index_;
  }

 private:
  size_t type_index_;
};

// T[N] -> T[]
class FixedArrayDecayOp : public ExprOp
{
 public:
  FixedArrayDecayOp(ExprOp *prev, Type *to)
   : ExprOp(prev, to, VK::rvalue)
  {}

  DECLARE_EXPROP(FixedArrayDecay);
};

#undef DECLARE_EXPROP
#undef EXPROP_MAP

}

#endif // _include_spcomp_expression_ops_h_
