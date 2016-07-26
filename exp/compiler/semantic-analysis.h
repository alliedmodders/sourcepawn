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
#ifndef _include_semantic_analysis_h_
#define _include_semantic_analysis_h_

#include "ast.h"

namespace sp {

class CompileContext;
class PoolAllocator;
class TranslationUnit;
class TypeManager;

class SemanticAnalysis : public StrictAstVisitor
{
 public:
  SemanticAnalysis(CompileContext &cc, TranslationUnit *unit);

  bool analyze();

 public:
  void visitFunctionStatement(FunctionStatement *node) override;
  void visitBlockStatement(BlockStatement *node) override;
  void visitExpressionStatement(ExpressionStatement *node) override;
  void visitCallExpr(CallExpr *node) override;
  void visitNameProxy(NameProxy *node) override;
  void visitStringLiteral(StringLiteral *node) override;
  void visitReturnStatement(ReturnStatement *node) override;

private:
  void analyzeShadowedFunctions(FunctionSymbol *sym);
  void checkForwardedFunction(FunctionStatement *forward, FunctionStatement *impl); 
  bool matchForwardSignatures(FunctionSignature *fwdSig, FunctionSignature *implSig);
  bool matchForwardReturnTypes(Type *fwdRetType, Type *implRetType);

  void checkCall(FunctionSignature *sig, ExpressionList *args);

  // Visit for any kind of value. This is guaranteed to not rewrite the
  // expression, so it returns a type instead.
  Type *visitForValue(Expression *expr);

  // Turn l-values into r-values.
  Expression *visitForRValue(Expression *expr);

 private:
  struct FuncState : StackLinked<FuncState>
  {
    FunctionNode *fun;
    FunctionSignature *sig;

    FuncState(FuncState **prev, FunctionNode *node)
     : StackLinked<FuncState>(prev),
       fun(node),
       sig(node->signature())
    {}
  };

 private:
  CompileContext &cc_;
  PoolAllocator &pool_;
  TypeManager *types_;
  TranslationUnit *tu_;

  FuncState *funcstate_;
};

}

#endif // _include_semantic_analysis_h_
