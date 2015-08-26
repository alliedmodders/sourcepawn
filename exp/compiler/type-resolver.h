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
#ifndef _include_spcomp_type_resolver_h_
#define _include_spcomp_type_resolver_h_

#include "ast.h"
#include "scopes.h"
#include "symbols.h"
#include "constant-evaluator.h"
#include "types.h"
#include <am-deque.h>

namespace sp {

using namespace ke;

class TranslationUnit;

class TypeSpecHelper;

class TypeResolver
 : public StrictAstVisitor,
   public ConstantResolver
{
 public:
  TypeResolver(CompileContext &cc);

  bool analyze();

  Type *resolveTypeOfVar(VariableSymbol *sym) override;
  bool resolveVarAsConstant(VariableSymbol *sym, BoxedValue *out) override;
  BoxedValue resolveValueOfConstant(ConstantSymbol *sym) override;
  void visitVarDecl(VarDecl *node) override;
  void visitEnumConstant(EnumConstant *node) override;
  void visitEnumStatement(EnumStatement *node) override;
  void visitMethodmapDecl(MethodmapDecl *methodmap) override;
  void visitFieldDecl(FieldDecl *decl) override;
  void visitPropertyDecl(PropertyDecl *decl) override;
  void visitMethodDecl(MethodDecl *decl) override;
  void visitFunctionStatement(FunctionStatement *node) override;
  void visitTypedefDecl(TypedefDecl *node) override;
  void visitUnsafeCastExpr(UnsafeCastExpr *expr) override;
  void visitCallNewExpr(CallNewExpr *expr) override;
  void visitNewArrayExpr(NewArrayExpr *expr) override;
  void visitTypesetDecl(TypesetDecl *decl) override;

  void addPending(AstNode *node) {
    work_queue_.append(node);
  }

  Type *applyConstQualifier(TypeSpecifier *spec, Type *type, TypeSpecHelper *helper);
  bool checkArrayInnerType(TypeSpecifier *spec, Type *type);

  void verifyTypeset(TypesetDecl *decl);

 private:
  EnumType *resolveMethodmapParentType(NameProxy *proxy);
  void visitFunction(FunctionNode *node);

 private:
  static const int kRankUnvisited;

  FunctionType *maybeResolveFunction(FunctionSignature *sig);

  bool resolveConstantArraySize(TypeSpecifier *spec, Expression *expr, int *outp);
  void updateComputedRankSize(Vector<int> &out, size_t rank, int size);
  void computeFixedArrayLiteralDimensions(ArrayLiteral *root,
                                          size_t rank,
                                          size_t highestUnknownRank,
                                          Vector<int> &out);
  Vector<int> fixedArrayLiteralDimensions(TypeSpecifier *spec, ArrayLiteral *lit);

  Type *resolveTypeIfNeeded(TypeExpr &te) {
    if (te.resolved())
      return te.resolved();
    return resolveType(te);
  }

  void resolveTypesInSignature(FunctionSignature *sig);
  void resolveConstant(ConstantSymbol *sym);
  Type *resolveType(TypeExpr &te, TypeSpecHelper *helper = nullptr, const Vector<int> *arrayInitData = nullptr);
  Type *resolveBaseType(TypeSpecifier *spec);
  Type *resolveNameToType(NameProxy *proxy);
  Type *resolveArrayComponentTypes(TypeSpecifier *spec,
                                   Type *type,
                                   const Vector<int> *arrayInitData = nullptr);
  bool resolveEnumConstantValue(EnumConstant *cs, int *outp);

 private:
  PoolAllocator &pool_;
  CompileContext &cc_;

  Deque<AstNode *> work_queue_;

  Vector<EnumConstant *> enum_constant_stack_;
};

// Certain keywords, like "const", may need contextual information to finish
// resolving. Callsites may provide information and callbacks via
// TypeSpecHelper.
class TypeSpecHelper
{
 public:
  // Called when a const-qualifier might be ambiguous, i.e. when the provided
  // type does not automatically assume responsibility for handling the meaning
  // of "const". Currently, this applies to any use of "const" with a type
  // other than an array.
  //
  // Return false to indicate that "const" has no meaning in this context
  // and should be ignored. Otherwise, the provider is responsible for
  // interpreting its meaning.
  virtual bool receiveConstQualifier(CompileContext &cc, const SourceLocation &constLoc, Type *type) {
    return false;
  }
  virtual const Vector<int> *arrayInitData() const {
    return nullptr;
  }
};

class VarDeclSpecHelper : public TypeSpecHelper
{
 public:
  VarDeclSpecHelper(VarDecl *decl, const Vector<int> *arrayInitData);

  bool receiveConstQualifier(CompileContext &cc, const SourceLocation &constLoc, Type *type) override;
  const Vector<int> *arrayInitData() const override;

 private:
  VarDecl *decl_;
  const Vector<int> *array_init_;
};

}

#endif // _include_spcomp_type_resolver_h_
