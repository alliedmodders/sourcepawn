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
#ifndef _include_spcomp2_semantic_analysis_h_
#define _include_spcomp2_semantic_analysis_h_

#include "compile-context.h"
#include "ast.h"
#include "scopes.h"
#include "symbols.h"
#include "hir.h"
#include "label.h"
#include "svalues.h"

namespace sp {

// Semantic analysis occurs after all names and types have been bound. It
// performs the important task of validating the semantics and type safety
// of a parse tree.
// 
// The semantic analysis phase also produces an IR called HIR. HIR is produced
// on a statement by statement basis; the result of analyzing each statement
// is the root of an expression tree encoded in HIR. The intention of HIR is
// to decompose complex operations into smaller components; for example, 
// (5 + 6.3) requires an implicit conversion that would be difficult to
// express with the AST.
//
// SemanticAnalysis should be derived into a class which is capable of emitting
// HIR into bytecode. Originally, SemA was designed to produce bytecode after
// each statement, discarding the HIR buffer each time. Now it is more flexible
// and HIR can be emitted on a statement or function basis.
class SemanticAnalysis : public AstVisitor
{
 protected:
  SemanticAnalysis(CompileContext &cc, TranslationUnit *unit);
  SemanticAnalysis(SemanticAnalysis *parent, FunctionStatement *node);

  bool analyze(ParseTree *tree);
  bool analyze(FunctionStatement *node);

 public:
  void visitFunctionStatement(FunctionStatement *node);
  void visitBlockStatement(BlockStatement *node);
  void visitVarDecl(VarDecl *node);
  void visitTokenLiteral(TokenLiteral *node);
  void visitIntegerLiteral(IntegerLiteral *node);
  void visitReturnStatement(ReturnStatement *node);
  void visitNameProxy(NameProxy *name);
  void visitExpressionStatement(ExpressionStatement *node);
  void visitCallExpr(CallExpr *node);
  void visitBinaryExpression(BinaryExpression *node);
  void visitAssignment(Assignment *node);
  void visitIfStatement(IfStatement *node);
  void visitIncDecExpression(IncDecExpression *node);
  void visitForStatement(ForStatement *node);
  void visitWhileStatement(WhileStatement *node);
  void visitBreakStatement(BreakStatement *node);
  void visitContinueStatement(ContinueStatement *node);
  void visitUnaryExpression(UnaryExpression *node);
  void visitTernaryExpression(TernaryExpression *node);
  void visitIndexExpression(IndexExpression *node);

 public:
  // These functions must be implemented by derived classes for handling
  // construction of the intermediate or final object layout.

  // Called in global scopes only.
  virtual void emit_native(FunctionStatement *node) = 0;
  virtual void emit_function(FunctionStatement *node) = 0;

  // Called in either global or local scopes.
  virtual void emit_variable(VarDecl *node, HIR *init) = 0;
  virtual void emit_bind(Label *label) = 0;
  virtual void emit_jump(Label *label) = 0;

  // Called in local scopes only.
  virtual void emit_prologue(FunctionStatement *node) = 0;
  virtual void emit_epilogue() = 0;
  virtual void emit_return(ReturnStatement *node, HIR *value = nullptr) = 0;
  virtual void emit_unwind(Scope *stop) = 0;
  virtual void emit_statement(ExpressionStatement *stmt, HIR *hir) = 0;
  virtual void emit_hirlist(HIRList *output) = 0;

  // Tells the underlying emitter that we're about to begin HIR construction
  // for a statement. This is designed to enter and leave pool scopes, and can
  // be overridden to save HIR across statements.
  virtual void begin_hir(PoolScope &pool_scope) {
    // HIR today, gone tomorrow
    pool_scope.enter(pool_);
  }

#if 0
  void visitFloatLiteral(FloatLiteral *node);
  void visitEnumStatement(EnumStatement *node);
  void visitSwitchStatement(SwitchStatement *node);

  void visitStringLiteral(StringLiteral *node);
  void visitArrayLiteral(ArrayLiteral *node);
#endif

 private:
  bool checkArgumentCount(FunctionType *type, unsigned actual);

 private:
  enum CoercionKind {
    Coerce_Arg,
    Coerce_Return,
    Coerce_Assign
  };

  HIR *coerce(HIR *from, Type *to, CoercionKind kind);
  Type *coercionType(TokenKind token, HIR *left, HIR *right);

  void visitForTest(HIRList *output, Expression *expr, Label *trueBranch, Label *falseBranch, Label *fallthrough);
  void set(VariableSymbol *sym);
  HIR *binary(AstNode *node, TokenKind token, HIR *left, HIR *right);
  bool lvalue(Expression *expr, LValue *outp);
  HIR *rvalue(Expression *expr);
  bool svalue(Expression *expr, SValue *outp);

  // bool typeCheckArray(const SourcePosition &pos,
  //           Handle<Type> argGiven,
  //           Handle<Type> argActual,
  //           CoercionKind kind);

 private:
  // Used to target break/catch statements.
  class LoopScope
  {
    Scope *scope_;
    Label *bk_;
    Label *ct_;
    LoopScope **prevp_;
    LoopScope *prev_;

   public:
    LoopScope(Scope *scope, Label *bk, Label *ct, LoopScope **prevp)
     : scope_(scope),
       bk_(bk),
       ct_(ct),
       prevp_(prevp),
       prev_(*prevp)
    {
      *prevp_ = this;
    }

    ~LoopScope()
    {
      *prevp_ = prev_;
    }

    Label *break_() {
      return bk_;
    }
    Label *continue_() {
      return ct_;
    }
    Scope *scope() {
      return scope_;
    }
  };
  
 protected:
  PoolAllocator &pool_;
  CompileContext &cc_;
  TranslationUnit *unit_;
  Scope *scope_;
  FunctionType *fun_;
  LoopScope *loop_;

  enum ValueContext {
    kLValue,
    kRValue
  };
 
  // Set and unset during rvalue()/lvalue().
  HIR *hir_;
  LValue *outp_;
  ValueContext value_context_;
};

} // namespace ke

#endif // _include_spcomp2_semantic_analysis_h_
