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
#ifndef _include_spcomp_type_specifier_h_
#define _include_spcomp_type_specifier_h_

#include "types.h"
#include "token-kind.h"
#include "source-location.h"
#include "pool-allocator.h"

namespace sp {

namespace ast {
class NameProxy;
class Expression;
class FunctionSignature;
typedef PoolList<Expression*> ExpressionList;

// Type specifiers are technically part of the AST, however they are quite
// critical both to parsing and type resolution so we define them separately
// for clarity.
//
// The raw TypeSpecifier type is constructed during parsing. However, it is
// a huge data structure (over 120 bytes on x64), and we do not want to keep
// it in the AST if we can avoid it. The parser therefore attempts to resolve
// TypeSpecifiers to Type objects as early as possible.
class TypeSpecifier : public PoolObject
{
 public:
  enum Attrs {
    Const      = 0x01,
    Variadic   = 0x02,
    ByRef      = 0x04,
    SizedArray = 0x08,
    NewDecl    = 0x10,
    PostDims   = 0x20,
    Resolving  = 0x40
  };

 public:
  TypeSpecifier()
   : attrs_(0),
     resolver_(TOK_NONE)
  {
    dims_ = nullptr;
    assert(rank_ == 0);
  }

  const SourceLocation& startLoc() const {
    return start_loc_;
  }

  void setBaseLoc(const SourceLocation& loc) {
    setLocation(base_loc_, loc);
  }
  const SourceLocation& baseLoc() const {
    return base_loc_;
  }

  void setBuiltinType(TokenKind kind) {
    resolver_ = kind;
  }
  void setNamedType(TokenKind kind, ast::NameProxy* proxy) {
    resolver_ = kind;
    proxy_ = proxy;
  }
  void setFunctionType(FunctionSignature* signature) {
    resolver_ = TOK_FUNCTION;
    signature_ = signature;
  }
  void setResolvedBaseType(Type* type) {
    resolver_ = TOK_DEFINED;
    resolved_ = type;
  }

  bool needsBinding() const {
    switch (resolver()) {
    case TOK_NAME:
    case TOK_LABEL:
    case TOK_FUNCTION:
      return true;
    default:
      return dims() != nullptr;
    }
  }

  void setVariadic(const SourceLocation& loc) {
    attrs_ |= Variadic;
    setLocation(variadic_loc_, loc);
  }
  bool isVariadic() const {
    return !!(attrs_ & Variadic);
  }
  const SourceLocation& variadicLoc() const {
    assert(isVariadic());
    return variadic_loc_;
  }

  void setConst(const SourceLocation& loc) {
    attrs_ |= Const;
    setLocation(const_loc_, loc);
  }
  bool isConst() const {
    return !!(attrs_ & Const);
  }
  const SourceLocation& constLoc() const {
    assert(isConst());
    return const_loc_;
  }

  void setByRef(const SourceLocation& loc) {
    attrs_ |= ByRef;
    setLocation(sigil_loc_, loc);
  }
  bool isByRef() const {
    return !!(attrs_ & ByRef);
  }
  const SourceLocation& byRefLoc() const {
    assert(isByRef());
    return sigil_loc_;
  }

  // As a small space-saving optimization, we overlay dims with rank. In many
  // cases the array will have no sized ranks and we'll save allocating a
  // list of nulls - or, in the vast majority of cases - we'll have no arrays
  // at all and we save an extra word on the very common TypeSpecifier.
  void setRank(const SourceLocation& sigil, uint32_t aRank) {
    assert(!rank());
    rank_ = aRank;
    sigil_loc_ = sigil;
    assert(start_loc_.isSet());
  }
  void setDimensionSizes(const SourceLocation& sigil, ast::ExpressionList* dims) {
    assert(!rank());
    dims_ = dims;
    attrs_ |= SizedArray;
    sigil_loc_ = sigil;
    assert(start_loc_.isSet());
  }
  bool isArray() const {
    return rank() > 0;
  }
  const SourceLocation& arrayLoc() const {
    assert(isArray());
    return sigil_loc_;
  }

  ast::ExpressionList* dims() const {
    if (attrs_ & SizedArray)
      return dims_;
    return nullptr;
  }
  ast::Expression* sizeOfRank(uint32_t r) const {
    assert(r < rank());
    if (attrs_ & SizedArray)
      return dims_->at(r);
    return nullptr;
  }
  uint32_t rank() const {
    if (attrs_ & SizedArray)
      return dims_->size();
    return rank_;
  }

  TokenKind resolver() const {
    return resolver_;
  }
  ast::NameProxy* proxy() const {
    assert(resolver() == TOK_NAME || resolver() == TOK_LABEL);
    return proxy_;
  }
  FunctionSignature* signature() const {
    assert(resolver() == TOK_FUNCTION);
    return signature_;
  }
  Type* getResolvedBase() const {
    assert(resolver() == TOK_DEFINED);
    return resolved_;
  }

  bool isNewDecl() const {
    return !!(attrs_ & NewDecl);
  }
  void setNewDecl() {
    attrs_ |= NewDecl;
  }
  bool hasPostDims() const {
    return !!(attrs_ & PostDims);
  }
  void setHasPostDims() {
    attrs_ |= PostDims;
  }
  bool isFixedArray() const {
    return isNewDecl() && hasPostDims();
  }

  bool isOldDecl() const {
    return !isNewDecl();
  }

  void unsetHasPostDims() {
    attrs_ &= ~PostDims;
  }

  // For reparse_decl().
  void resetWithAttrs(uint32_t attrMask) {
    uint32_t saveAttrs = attrs_ & attrMask;
    *this = TypeSpecifier();
    attrs_ = saveAttrs;
  }
  void resetArray() {
    rank_ = 0;
    dims_ = nullptr;
    attrs_ &= ~(SizedArray|PostDims);
  }

  void setResolved() {
    // We don't care if isResolving() is true, since sometimes we know the type
    // earlier than resolution. We do unset the resolving bit however.
    attrs_ &= ~Resolving;
  }
  // Mark that we're resolving something, so we can detect recursive types.
  void setResolving() {
    attrs_ |= Resolving;
  }
  bool isResolving() const {
    return !!(attrs_ & Resolving);
  }

 private:
  void setLocation(SourceLocation& where, const SourceLocation& loc) {
    where = loc;
    if (!start_loc_.isSet())
      start_loc_ = loc;
  }

 private:
  SourceLocation start_loc_;
  SourceLocation base_loc_;
  SourceLocation const_loc_;
  SourceLocation variadic_loc_;
  SourceLocation sigil_loc_;
  uint32_t attrs_;
  union {
    uint32_t rank_;
    ast::ExpressionList* dims_;
  };
  TokenKind resolver_;
  union {
    ast::NameProxy* proxy_;
    FunctionSignature* signature_;
    Type* resolved_;
  };
};

// This is the structure that is stored in the AST whenever we want to store
// a type expression.
class TypeExpr
{
 public:
  // This constructor should not be used, it's for classes which assign their
  // type-exprs after being constructed.
  TypeExpr()
   : type_(nullptr),
     spec_(nullptr)
  {}
  explicit TypeExpr(TypeSpecifier* spec)
   : type_(nullptr),
     spec_(spec)
  {}
  explicit TypeExpr(Type* type)
   : type_(type),
     spec_(nullptr)
  {}

  bool isResolved() const {
    return !!type_;
  }
  bool isResolving() const {
    return spec_ && spec_->isResolving();
  }
  void setResolved(Type* type) {
    assert(!type_ || type_->isUnresolvable() || type_->isImplicitInt());
    if (spec_)
      spec_->setResolved();
    type_ = type;
    spec_ = nullptr;
  }
  Type* resolved() const {
    assert(type_ || spec_);
    return type_;
  }
  TypeSpecifier* spec() const {
    assert(type_ || spec_);
    return spec_;
  }

 private:
  Type* type_;
  TypeSpecifier* spec_;
};

} // namespace ast
} // namespace sp

#endif // _include_spcomp_type_specifier_h_
