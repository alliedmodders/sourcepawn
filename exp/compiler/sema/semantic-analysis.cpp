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
#include "compile-context.h"
#include "semantic-analysis.h"
#include "scopes.h"
#include "symbols.h"
#include <amtl/am-linkedlist.h>

namespace sp {

using namespace ke;
using namespace ast;

// :TODO: constant folding

SemanticAnalysis::SemanticAnalysis(CompileContext &cc, TranslationUnit *tu)
 : cc_(cc),
   pool_(cc.pool()),
   types_(cc.types()),
   tu_(tu),
   fs_(nullptr),
   loop_depth_(0)
{
}

sema::Program*
SemanticAnalysis::analyze()
{
  if (!walkAST())
    return nullptr;

  sema::Program* program = new (pool_) sema::Program;
  program->functions = ke::Move(global_functions_);
  program->globals = ke::Move(global_vars_);
  return program;
}

bool
SemanticAnalysis::walkAST()
{
  ParseTree *tree = tu_->tree();
  StatementList *statements = tree->statements();
  for (size_t i = 0; i < statements->length(); i++) {
    Statement *stmt = statements->at(i);
    switch (stmt->kind()) {
      case AstKind::kFunctionStatement:
      {
        FunctionStatement* fun = stmt->toFunctionStatement();
        visitFunctionStatement(fun);
        break;
      }
      case AstKind::kVarDecl:
      {
        VarDecl* decl = stmt->toVarDecl();
        visitVarDecl(decl);
        break;
      }
      case AstKind::kRecordDecl:
        // Type declarations don't have semantic checks.
        continue;
      default:
        cc_.report(stmt->loc(), rmsg::unimpl_kind) <<
          "sema-ast-walk" << stmt->kindName();
        break;
    }
    if (!cc_.canContinueProcessing())
      return false;
  }
  return cc_.phasePassed();
}

#if 0
void
SemanticAnalysis::visitExpressionStatement(ExpressionStatement *node)
{
  Expression *expr = node->expr();

  expr->accept(this);

  // if (!expr->hasSideEffects())
  //   cc_.report(node->loc(), rmsg::expr_has_no_side_effects);
}
#endif

#if 0
void
SemanticAnalysis::visitCallExpr(CallExpr *node)
{
  // :TODO: we must verify that the callee is an implemented scripted func.
  Expression *callee = visitForRValue(node->callee());
  if (!callee)
    return;
  if (!callee->type()->isFunction()) {
    cc_.report(node->loc(), rmsg::callee_is_not_a_function)
      << callee->type();
    return;
  }
  node->setCallee(callee);

  FunctionSignature *sig = callee->type()->toFunction()->signature();
  checkCall(sig, node->arguments());

  Type *returnType = sig->returnType().resolved();
  node->setOutput(returnType, VK::rvalue);

  // We mark calls as always having side effects.
  node->setHasSideEffects();
}
#endif

} // namespace sp
