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
#ifndef _include_spcomp2_hir_h_
#define _include_spcomp2_hir_h_

#include "label.h"
#include "types.h"
#include "symbols.h"

namespace sp {

#define HIR_OPS(_)            \
  _(Jump)                     \
  _(Bind)                     \
  _(Function)                 \
  _(Boolean)                  \
  _(Integer)                  \
  _(Float)                    \
  _(Local)                    \
  _(Global)                   \
  _(Call)                     \
  _(Binary)                   \
  _(Store)                    \
  _(PostIncDec)               \
  _(Not)                      \
  _(Invert)                   \
  _(Negate)                   \
  _(Ternary)                  \
  _(Index)                    \
  _(CompareAndJump)           \
  _(AddressOf)

// Forward declarations.
#define _(name)  class H##name;
HIR_OPS(_)
#undef _

class HIR;
class HIRVisitor;
class AstNode;

typedef PoolList<HIR *> HIRList;

class HIR : public PoolObject
{
 public:
  enum HOp {
#define _(name) k##name,
    HIR_OPS(_)
#undef _
    kTotalOps
  };

 public:
  HIR(AstNode *node, Type *type)
   : node_(node),
     type_(type)
  {
  }

  Type *type() {
    return type_;
  }
  AstNode *node() const {
    return node_;
  }

  virtual HOp op() const = 0;
  virtual const char *opname() const = 0;
  virtual void accept(HIRVisitor *visitor) = 0;

#define CASTER(name)                                          \
  bool is##name() {                                           \
    return op() == k##name;                                   \
  }                                                           \
  H##name *to##name() {                                       \
    assert(is##name());                                       \
    return (H##name *)this;                                   \
  }                                                           \
  H##name *as##name() {                                       \
    if (!is##name())                                          \
      return nullptr;                                         \
    return to##name();                                        \
  }
  HIR_OPS(CASTER)
#undef CASTER

 private:
  AstNode *node_;

 protected:
  Type *type_;
};

class HLabel : public Label, public PoolObject
{
 public:
  HLabel() {
  }
};

class BytecodeEmitter;

class HIRVisitor
{
 public:
#define _(name)                             \
  virtual void visit##name(H##name *hir) {  \
    assert(false);                          \
  }
  HIR_OPS(_)
#undef _
};

#define DEFINE_HIR(name)                        \
    virtual HOp op() const {                    \
        return k##name;                         \
    }                                           \
    virtual const char *opname() const {        \
        return "H" #name;                       \
    }                                           \
    virtual void accept(HIRVisitor *visitor) {  \
        visitor->visit##name(this);             \
    }

// Represents a location in memory that can be stored to.
class LValue
{
 public:
  enum Kind {
    Error,
    Variable,
    BaseIndex
  };

  LValue() {
    kind_ = Error;
  }
  LValue(VariableSymbol *sym) {
    kind_ = Variable;
    u.var.sym = sym;
  }
  LValue(HIR *base, HIR *index) {
    kind_ = BaseIndex;
    u.elem.base = base;
    u.elem.index = index;
  }

  Kind kind() const {
    return kind_;
  }
  bool isVariable() const {
    return kind_ == Variable;
  }
  bool isBaseIndex() const {
    return kind_ == BaseIndex;
  }
  VariableSymbol *sym() const {
    assert(isVariable());
    return u.var.sym;
  }
  HIR *base() const {
    assert(isBaseIndex());
    return u.elem.base;
  }
  HIR *index() const {
    assert(isBaseIndex());
    return u.elem.index;
  }
  Type *reftype() const {
    if (kind() == Variable)
      return sym()->type();
    return base()->type();
  }
  Type *type() const {
    if (kind() == Variable) {
      if (sym()->type()->isReference())
        return sym()->type()->toReference()->contained();
      return sym()->type();
    }
    return base()->type()->toArray()->contained();
  }

 private:
  Kind kind_;
  union {
    struct {
      VariableSymbol *sym;
    } var;
    struct {
      HIR *base;
      HIR *index;
    } elem;
  } u;
};


class HFunction : public HIR
{
 public:
  HFunction(AstNode *node, FunctionSymbol *sym)
   : HIR(node, nullptr /* :TODO: sym->type()*/),
     sym_(sym)
  {
  }

  DEFINE_HIR(Function);
  FunctionSymbol *sym() const {
    return sym_;
  }

 private:
  FunctionSymbol *sym_;
};

class HBoolean : public HIR
{
 public:
  HBoolean(AstNode *node, Type *type, TokenKind tok)
   : HIR(node, type),
     value_(tok)
  {
  }

  DEFINE_HIR(Boolean);
  TokenKind value() const {
    return value_;
  }

 private:
  TokenKind value_;
};

class HInteger : public HIR
{
 public:
  HInteger(AstNode *node, Type *type, int value)
   : HIR(node, type),
     value_(value)
  {
  }

  DEFINE_HIR(Integer);
  int value() const {
    return value_;
  }

 private:
  int value_;
};

class HFloat : public HIR
{
 public:
  HFloat(AstNode *node, Type *type, float value)
   : HIR(node, type),
     value_(value)
  {
  }

  DEFINE_HIR(Float);
  float value() const {
    return value_;
  }

 private:
  float value_;
};

class HLocal : public HIR
{
 public:
  HLocal(AstNode *node, Type *type, VariableSymbol *sym)
   : HIR(node, type),
     sym_(sym)
  {
  }

  DEFINE_HIR(Local);

  VariableSymbol *sym() const {
    return sym_;
  }

 private:
  VariableSymbol *sym_;
};

class HGlobal : public HIR
{
 public:
  HGlobal(AstNode *node, VariableSymbol *sym)
   : HIR(node, sym->type()),
     sym_(sym)
  {
  }

  DEFINE_HIR(Global);
  VariableSymbol *sym() const {
    return sym_;
  }

 private:
  VariableSymbol *sym_;
};

class HIndex : public HIR
{
 public:
  HIndex(AstNode *node, Type *type, HIR *left, HIR *right)
   : HIR(node, type),
     left_(left),
     right_(right)
  {
  }

  DEFINE_HIR(Index);

  HIR *left() const {
    return left_;
  }
  HIR *right() const {
    return right_;
  }

 private:
  HIR *left_;
  HIR *right_;
};


class HCall : public HIR
{
  public:
  HCall(AstNode *node, Type *type, HIR *callee, HIRList *args)
    : HIR(node, type),
    callee_(callee),
    args_(args)
  {
  }

  DEFINE_HIR(Call);

  HIR *callee() const {
    return callee_;
  }
  HIRList *args() const {
    return args_;
  }

  private:
  HIR *callee_;
  HIRList *args_;
};

class HBinary : public HIR
{
 public:
  HBinary(AstNode *node, Type *type, TokenKind token, HIR *left, HIR *right)
    : HIR(node, type),
    token_(token),
    left_(left),
    right_(right)
  {
  }

  DEFINE_HIR(Binary);
  TokenKind token() const {
    return token_;
  }
  HIR *left() const {
    return left_;
  }
  HIR *right() const {
    return right_;
  }

 private:
  TokenKind token_;
  HIR *left_;
  HIR *right_;
};

class HStore : public HIR
{
 public:
  HStore(AstNode *node, Type *type, const LValue &lval, HIR *rval)
   : HIR(node, type),
     kind_(TOK_ASSIGN),
     lval_(lval),
     rval_(rval)
  {
  }

  HStore(AstNode *node, Type *type, TokenKind kind, const LValue &lval, HIR *rval)
   : HIR(node, type),
     kind_(kind),
     lval_(lval),
     rval_(rval)
  {
  }

  DEFINE_HIR(Store);

  const LValue &lval() const {
    return lval_;
  }
  HIR *rval() const {
    return rval_;
  }
  TokenKind kind() const {
    return kind_;
  }

 private:
  TokenKind kind_;
  LValue lval_;
  HIR *rval_;
};

class HPostIncDec : public HIR
{
 public:
  HPostIncDec(AstNode *node, Type *type, TokenKind kind, const LValue &lval)
   : HIR(node, type),
     kind_(kind),
     lval_(lval)
  {
  }

  DEFINE_HIR(PostIncDec);

  const LValue &lval() const {
    return lval_;
  }
  TokenKind kind() const {
    return kind_;
  }

 private:
  TokenKind kind_;
  LValue lval_;
};

class HNegate : public HIR
{
 public:
  HNegate(AstNode *node, HIR *hir)
   : HIR(node, hir->type()),
     expr_(hir)
  {
  }

  DEFINE_HIR(Negate);
  HIR *expr() const {
    return expr_;
  }

 private:
  HIR *expr_;
};

class HInvert : public HIR
{
 public:
  HInvert(AstNode *node, HIR *hir)
   : HIR(node, hir->type()),
     expr_(hir)
  {
  }

  DEFINE_HIR(Invert);
  HIR *expr() const {
    return expr_;
  }

 private:
  HIR *expr_;
};

class HNot : public HIR
{
 public:
  HNot(AstNode *node, Type *type, HIR *hir)
   : HIR(node, type),
     expr_(hir)
  {
  }

  DEFINE_HIR(Not);
  HIR *expr() const {
    return expr_;
  }

 private:
  HIR *expr_;
};

class HJump : public HIR
{
 public:
  HJump(AstNode *node, HIR *test, bool jumpOnTrue, Label *target)
   : HIR(node, nullptr),
     test_(test),
     jump_on_true_(jumpOnTrue),
     target_(target)
  {
  }

  DEFINE_HIR(Jump);
  HIR *test() const {
    return test_;
  }
  bool jump_on_true() const {
    assert(test());
    return jump_on_true_;
  }
  Label *target() const {
    return target_;
  }

 private:
  HIR *test_;
  bool jump_on_true_;
  Label *target_;
};

class HBind : public HIR
{
 public:
  HBind(AstNode *node, Label *label)
   : HIR(node, nullptr),
     label_(label)
  {
  }

  DEFINE_HIR(Bind);
  Label *label() const {
    return label_;
  }

 private:
  Label *label_;
};

class HTernary : public HIR
{
 public:
  HTernary(AstNode *node, HIR *left, HIR *right)
   : HIR(node, left->type()),
     left_(left),
     right_(right)
  {
  }

  DEFINE_HIR(Ternary);

  HIRList *test() {
    return &test_;
  }
  Label *success() {
    return &success_;
  }
  Label *failure() {
    return &failure_;
  }
  Label *done() {
    return &done_;
  }
  HIR *left() const {
    return left_;
  }
  HIR *right() const {
    return right_;
  }

 private:
  HIRList test_;
  Label success_;
  Label failure_;
  Label done_;
  HIR *left_;
  HIR *right_;
};

class HCompareAndJump : public HIR
{
 public:
  HCompareAndJump(AstNode *node, TokenKind token, HIR *left, HIR *right, Label *target)
   : HIR(node, nullptr),
     token_(token),
     left_(left),
     right_(right),
     target_(target)
  {
  }

  DEFINE_HIR(CompareAndJump);
  TokenKind token() const {
    return token_;
  }
  HIR *left() const {
    return left_;
  }
  HIR *right() const {
    return right_;
  }
  Label *target() const {
    return target_;
  }

 private:
  TokenKind token_;
  HIR *left_;
  HIR *right_;
  Label *target_;
};

class HAddressOf : public HIR
{
 public:
  HAddressOf(AstNode *node, const LValue &lval)
   : HIR(node, lval.type()),
     lvalue_(lval)
  {
  }

  DEFINE_HIR(AddressOf);

  const LValue &lval() const {
    return lvalue_;
  }

 private:
  LValue lvalue_;
};

#undef DEFINE_HIR

} // namespace ke

#endif // _include_spcomp2_hir_h_
