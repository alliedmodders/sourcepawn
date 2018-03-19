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

#include "parser/ast.h"
#include "sema/expressions.h"
#include "sema/program.h"

namespace sp {

class CompileContext;
class PoolAllocator;
class TranslationUnit;
class TypeManager;

using namespace ast;

class SemanticAnalysis
{
 public:
  SemanticAnalysis(CompileContext &cc, TranslationUnit *unit);

  sema::Program* analyze();

 private:
  bool walkAST();

  void visitFunctionStatement(FunctionStatement* node);
  void visitBlockStatement(BlockStatement* node);
  void visitStatement(Statement* node);
  void visitReturnStatement(ReturnStatement* node);

  sema::Expr* visitExpression(Expression* node);
  sema::ConstValue* visitIntegerLiteral(IntegerLiteral* node);

 private:
  void analyzeShadowedFunctions(FunctionSymbol *sym);
  void checkForwardedFunction(FunctionStatement *forward, FunctionStatement *impl); 
  bool matchForwardSignatures(FunctionSignature *fwdSig, FunctionSignature *implSig);
  bool matchForwardReturnTypes(Type *fwdRetType, Type *implRetType);

  void checkCall(FunctionSignature *sig, ExpressionList *args);

  enum class Coercion {
    Return
  };
  sema::Expr* coerce(sema::Expr* from, Type* to, Coercion context);

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

  ke::Vector<ast::FunctionStatement*> global_functions_;
};

} // namespace sp

#endif // _include_semantic_analysis_h_
