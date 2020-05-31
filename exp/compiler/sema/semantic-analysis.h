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
struct EvalContext;
struct TernaryContext;

using namespace ast;

class SemanticAnalysis
{
 public:
  SemanticAnalysis(CompileContext& cc, TranslationUnit* unit);

  sema::Program* analyze();

 private:
  bool walkAST();

  void visitFunctionStatement(FunctionStatement* node);
  void visitBlockStatement(BlockStatement* node);
  void visitStatement(Statement* node);
  void visitReturnStatement(ReturnStatement* node);
  void visitExpressionStatement(ExpressionStatement* node);
  void visitVarDecl(VarDecl* node);
  void visitWhileStatement(WhileStatement* node);
  void visitForStatement(ForStatement* node);
  void visitIfStatement(IfStatement* node);
  void visitBreakStatement(BreakStatement* node);
  void visitContinueStatement(ContinueStatement* node);
  void visitSwitchStatement(SwitchStatement* node);

  // Expression handling.
  sema::Expr* visitExpression(Expression* node);
  sema::ConstValueExpr* visitIntegerLiteral(IntegerLiteral* node);
  sema::BinaryExpr* visitBinaryExpression(BinaryExpression* node);
  sema::CallExpr* visitCallExpression(ast::CallExpression* node);
  sema::Expr* visitNameProxy(ast::NameProxy* node);
  sema::Expr* visitUnaryExpression(ast::UnaryExpression* node);
  sema::Expr* visitStringLiteral(ast::StringLiteral* node);
  sema::Expr* visitCharLiteral(ast::CharLiteral* node);
  sema::Expr* visitIncDec(ast::IncDecExpression* node);
  sema::Expr* visitIndex(ast::IndexExpression* node);
  sema::Expr* visitAssignment(ast::Assignment* node);
  sema::Expr* visitNewArray(ast::NewArrayExpr* node);
  sema::Expr* visitTernary(ast::TernaryExpression* node);
  sema::Expr* visitSizeof(ast::SizeofExpression* node);
  sema::Expr* visitViewAs(ast::ViewAsExpression* node);

 private:
  void analyzeShadowedFunctions(FunctionSymbol* sym);
  void checkForwardedFunction(FunctionStatement* forward, FunctionStatement* impl);
  bool matchForwardSignatures(FunctionSignature* fwdSig, FunctionSignature* implSig);
  bool matchForwardReturnTypes(Type* fwdRetType, Type* implRetType);

  // Same as visitExpression, but only returns l-values.
  sema::LValueExpr* visitLValue(Expression* node);

  bool coerce(EvalContext& ec);

  // Arguments are so complicated in SP that we define a special coercion
  // helper for them.
  sema::Expr* coerce_arg(ast::Expression* expr, Type* to);
  sema::Expr* coerce_vararg(sema::LValueExpr* expr, Type* to);

  // Similarly, the dreaded ternary operator is also quite complicated.
  bool coerce_ternary(TernaryContext& tc);

  // Do not call any of these directly.
  bool coerce_array(EvalContext& ec);
  bool coerce_ref(EvalContext& ec);
  bool coerce_primitive(EvalContext& ec);
  bool coerce_to_char(EvalContext& ec);
  bool no_conversion(EvalContext& ec);
  Type* arrayOrSliceType(EvalContext& ec, sema::IndexExpr** out);
  sema::Expr* lvalue_to_rvalue(sema::LValueExpr* expr);

  sema::Expr* initializer(ast::Expression* expr, Type* type);
  sema::Expr* struct_initializer(ast::StructInitializer* expr, Type* type);
  sema::Expr* array_initializer(ast::ArrayLiteral* expr, Type* type);
  sema::Expr* infer_array_initializer(VarDecl* node);

  bool isValidNewArrayInitializer(Type* from, Type* to);
  static bool CompareNonArrayTypesExactly(Type* a, Type* b);

 private:
  enum class ReturnStatus {
    None,   // No return statements.
    Mixed,  // Some returns.
    All     // All paths return.
  };

  struct FuncState : StackLinked<FuncState>
  {
    FuncState(FuncState** prev, FunctionNode* node)
     : StackLinked<FuncState>(prev),
       fun(node),
       sig(node->signature()),
       return_status(ReturnStatus::None)
    {}

    FunctionNode* fun;
    FunctionSignature* sig;

    // This tracks how the current control-flow path returns.
    ReturnStatus return_status;
  };

 private:
  CompileContext& cc_;
  PoolAllocator& pool_;
  TypeManager* types_;
  TranslationUnit* tu_;

  FuncState* fs_;

  std::vector<ast::FunctionStatement*> global_functions_;
  std::vector<ast::VarDecl*> global_vars_;

  size_t loop_depth_;
};

} // namespace sp

#endif // _include_semantic_analysis_h_
