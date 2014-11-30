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
#include "layout.h"
#include <am-deque.h>

namespace sp {

using namespace ke;

class TranslationUnit;

class TypeResolver
 : public PartialAstVisitor,
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

  void addPending(AstNode *node) {
    work_queue_.append(node);
  }

 private:
  EnumType *resolveMethodmapParentType(NameProxy *proxy);
  void visitFunction(FunctionNode *node);

 private:
  static const int kRankUnvisited;

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
  Type *resolveType(TypeExpr &te, const Vector<int> *arrayInitData = nullptr);
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

}

#endif // _include_spcomp_type_resolver_h_
