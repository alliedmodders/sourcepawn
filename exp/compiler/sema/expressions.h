// vim: set ts=2 sw=2 tw=99 et:
// 
// Copyright (C) 2012-2014 AlliedModders LLC and David Anderson
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
#ifndef _include_sourcepawn_sema_expressions_h_
#define _include_sourcepawn_sema_expressions_h_

#include "boxed-value.h"
#include "pool-allocator.h"

namespace sp {

class Type;
class FunctionSymbol;
class VariableSymbol;

namespace ast {
class Expression;
} // namespace ast

namespace sema {

#define SEMA_KIND_LIST(_) \
  _(Binary)               \
  _(Unary)                \
  _(Call)                 \
  _(ConstValue)           \
  _(NamedFunction)        \
  _(Var)                  \
  _(TrivialCast)          \
  _(String)               \
  _(IncDec)               \
  _(StructInit)           \
  _(Index)                \
  _(Load)                 \
  _(Slice)                \
  /* terminator */

// Forward declarations.
#define _(name) class name##Expr;
SEMA_KIND_LIST(_)
#undef _

enum class ExprKind
{
#define _(name) name,
  SEMA_KIND_LIST(_)
#undef _
  Sentinel
};

class LValueExpr;

class Expr : public PoolObject
{
public:
  explicit Expr(ast::Expression* node, Type* type)
    : node_(node),
      type_(type)
  {}

  Type* type() const {
    return type_;
  }
  ast::Expression* src() const {
    return node_;
  }
  virtual ExprKind kind() const = 0;
  virtual const char* prettyName() const = 0;

  virtual bool isConstant() const {
    return false;
  }
  virtual bool getBoxedValue(BoxedValue* out) const {
    return false;
  }
  virtual bool getConstantInt32(int32_t* value) const {
    return false;
  }
  virtual LValueExpr* asLValueExpr() {
    return nullptr;
  }
  virtual bool hasSideEffects() const {
    return false;
  }

#define _(name)                                 \
  virtual name##Expr* as##name##Expr() {        \
    return nullptr;                             \
  }                                             \
  virtual name##Expr* to##name##Expr() {        \
    assert(false);                              \
    return nullptr;                             \
  }
  SEMA_KIND_LIST(_)
#undef _

protected:
  ast::Expression* node_;
  Type* type_;
};

typedef PoolList<Expr*> ExprList;

#define DECLARE_SEMA(name)                  \
  ExprKind kind() const override {          \
    return ExprKind::name;                  \
  }                                         \
  name##Expr* as##name##Expr() override {   \
    return this;                            \
  }                                         \
  name##Expr* to##name##Expr() override {   \
    return this;                            \
  }                                         \
  const char* prettyName() const override { \
    return #name "Expr";                    \
  }

class ConstValueExpr final : public Expr
{
public:
  explicit ConstValueExpr(ast::Expression* node, Type* type, const BoxedValue& value)
   : Expr(node, type),
     value_(value)
  {}

  DECLARE_SEMA(ConstValue)

  const BoxedValue& value() const {
    return value_;
  }
  bool isConstant() const override {
    return true;
  }
  bool getBoxedValue(BoxedValue* out) const override {
    *out = value_;
    return true;
  }
  bool getConstantInt32(int32_t* value) const override {
    if (!value_.isInteger())
      return false;
    if (!value_.toInteger().valueFitsInInt32())
      return false;
    *value = value_.toInteger().asSigned();
    return true;
  }

private:
  BoxedValue value_;
};

class BinaryExpr final : public Expr
{
 public:
  explicit BinaryExpr(ast::Expression* node,
                      Type* type,
                      TokenKind tok,
                      Expr* left,
                      Expr* right)
   : Expr(node, type),
     token_(tok),
     left_(left),
     right_(right)
  {}

  DECLARE_SEMA(Binary)

  TokenKind token() const {
    return token_;
  }
  Expr* left() const {
    return left_;
  }
  Expr* right() const {
    return right_;
  }
  bool hasSideEffects() const override {
    return left_->hasSideEffects() || right_->hasSideEffects();
  }

 private:
  TokenKind token_;
  Expr* left_;
  Expr* right_;
};

class UnaryExpr final : public Expr
{
 public:
  explicit UnaryExpr(ast::Expression* node,
                     Type* type,
                     TokenKind tok,
                     Expr* expr)
   : Expr(node, type),
     token_(tok),
     expr_(expr)
  {}

  DECLARE_SEMA(Unary)

  TokenKind token() const {
    return token_;
  }
  Expr* expr() const {
    return expr_;
  }
  bool hasSideEffects() const override {
    return expr_->hasSideEffects();
  }

 private:
  TokenKind token_;
  Expr* expr_;
};

class IncDecExpr final : public Expr
{
 public:
  explicit IncDecExpr(ast::Expression* node,
                      Type* type,
                      TokenKind tok,
                      LValueExpr* expr,
                      bool postfix)
   : Expr(node, type),
     token_(tok),
     expr_(expr),
     postfix_(postfix)
  {}

  DECLARE_SEMA(IncDec)

  TokenKind token() const {
    return token_;
  }
  LValueExpr* expr() const {
    return expr_;
  }
  bool prefix() const {
    return !postfix_;
  }
  bool postfix() const {
    return postfix_;
  }
  bool hasSideEffects() const override {
    return true;
  }

 private:
  TokenKind token_;
  LValueExpr* expr_;
  bool postfix_;
};

class TrivialCastExpr final : public Expr
{
 public:
  explicit TrivialCastExpr(ast::Expression* node,
                           Type* type,
                           Expr* expr)
   : Expr(node, type),
     expr_(expr)
  {}

  DECLARE_SEMA(TrivialCast)

  Expr* expr() const {
    return expr_;
  }
  bool hasSideEffects() const override {
    return expr_->hasSideEffects();
  }

 private:
  Expr* expr_;
};

class NamedFunctionExpr final : public Expr
{
 public:
  explicit NamedFunctionExpr(ast::Expression* node,
                             Type* type,
                             FunctionSymbol* sym)
   : Expr(node, type),
     sym_(sym)
  {}

  DECLARE_SEMA(NamedFunction)

  FunctionSymbol* sym() const {
    return sym_;
  }

 private:
  FunctionSymbol* sym_;
};

class CallExpr final : public Expr
{
 public:
  explicit CallExpr(ast::Expression* node,
                    Type* type,
                    Expr* callee,
                    ExprList* args)
   : Expr(node, type),
     callee_(callee),
     args_(args)
  {}

  DECLARE_SEMA(Call)

  Expr* callee() const {
    return callee_;
  }
  ExprList* args() const {
    return args_;
  }
  bool hasSideEffects() const override {
    return true;
  }

 private:
  Expr* callee_;
  ExprList* args_;
};

// LValue expressions compute an address that can be used as the left-hand side
// of an assignment. For local variables this is a stack address; for pointer
// types (references, dynamic arrays), it is the pointer in question. For
// fixed-length arrays, there is no difference between an l-value and r-value.
//
// Semantic Analysis uses l-values to express the complexity of SourcePawn's
// many frontend and VM oddities, as well as allowing future emitters to use
// a more simplified bytecode. For example, the current design allows the
// emitter to simply return an address for any LValueExpr, and then perform
// a load operation on that address. However, an emitter may also pattern-match
// common scenarios (like loading a local variable) to avoid actually computing
// an address.
//
// LValueExprs normally only appear in Loads, IncDecs, and Stores. However,
// they can also occur when it is not possible to compute an indirect address.
// For example a fixed-length array variable has no semantic indirection,
// whereas a dynamic array does. The backend emitter may choose to make both
// indirectly stored; however, semantically, the cases are different. Therefore
// the backend emitter has to decide exactly how to procure an address for each
// situation.
//
// To demonstrate,
//    int x[10];
//    int y[] = new int[10];
//    use_arrays(x, y);
//
// Will generate a Var(x) and Load(Var(y)) respectively, since the latter case
// has an lvalue-to-rvalue conversion.
//
// Note that in the above example, assigning to |y| is not supported. There
// is no support in the VM for making this safe. However, it is easily
// supported in our semantic analysis model, and:
//
//    Store(y, <Expr>)
//
// Would cause the emitter to store a pointer value at |y|, whereas:
//
//    Store(x, <Expr>)
//
// *Is* currently supported, and will emit a copy.
class LValueExpr : public Expr
{
 public:
  explicit LValueExpr(ast::Expression* node, Type* type)
   : Expr(node, type)
  {}

  LValueExpr* asLValueExpr() final {
    return this;
  }

  // Return the underlying type being stored, peeling away references.
  Type* storedType() const {
    return type_->isReference()
           ? type_->toReference()->inner()
           : type_;
  }
};

class VarExpr final : public LValueExpr
{
 public:
  explicit VarExpr(ast::Expression* node,
                   Type* type,
                   VariableSymbol* sym)
   : LValueExpr(node, type),
     sym_(sym)
  {}

  DECLARE_SEMA(Var)

  VariableSymbol* sym() const {
    return sym_;
  }

 private:
  VariableSymbol* sym_;
};

class IndexExpr final : public LValueExpr
{
 public:
  explicit IndexExpr(ast::Expression* node,
                      Type* type,
                      Expr* base,
                      Expr* index)
   : LValueExpr(node, type),
     base_(base),
     index_(index)
  {}

  DECLARE_SEMA(Index)

  Expr* base() const {
    return base_;
  }
  Expr* index() const {
    return index_;
  }
  bool hasSideEffects() const override {
    return base_->hasSideEffects() || index_->hasSideEffects();
  }

 private:
  Expr* base_;
  Expr* index_;
};

class StringExpr final : public Expr
{
 public:
  explicit StringExpr(ast::Expression* node,
                      Type* type,
                      Atom* literal)
   : Expr(node, type),
     literal_(literal)
  {}

  DECLARE_SEMA(String)

  Atom* literal() const {
    return literal_;
  }

 private:
  Atom* literal_;
};

class StructInitExpr final : public Expr
{
 public:
  explicit StructInitExpr(ast::Expression* node,
                          Type* type,
                          ExprList* exprs)
   : Expr(node, type),
     exprs_(exprs)
  {}

  DECLARE_SEMA(StructInit)

  bool isConstant() const override {
    return true;
  }
  ExprList* exprs() const {
    return exprs_;
  }

 private:
  ExprList* exprs_;
};

// Turn an l-value into an r-value. This can only be used on l-values for which
// references exist.
class LoadExpr : public Expr
{
 public:
  explicit LoadExpr(ast::Expression* node, Type* type, LValueExpr* expr)
   : Expr(node, type),
     lvalue_(expr)
  {}

  DECLARE_SEMA(Load)

  LValueExpr* lvalue() const {
    return lvalue_;
  }
  bool hasSideEffects() const override {
    return lvalue_->hasSideEffects();
  }

 private:
  LValueExpr* lvalue_;
};

// Create an array, valid for the duration of the current statement, that
// offers a view into another array.
class SliceExpr : public Expr
{
 public:
  explicit SliceExpr(ast::Expression* node, Type* type,
                     Expr* base, Expr* index)
   : Expr(node, type),
     base_(base),
     index_(index)
  {}

  DECLARE_SEMA(Slice)

  Expr* base() const {
    return base_;
  }
  Expr* index() const {
    return index_;
  }
  bool hasSideEffects() const override {
    return base_->hasSideEffects() || index_->hasSideEffects();
  }

 private:
  Expr* base_;
  Expr* index_;
};

#undef DECLARE_SEMA
#undef SEMA_KIND_LIST

} // namespace sema
} // namespace sp

#endif // _include_sourcepawn_sema_expressions_h_

