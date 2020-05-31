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
#include "coercion.h"

namespace sp {

using namespace ke;
using namespace ast;

void
SemanticAnalysis::visitFunctionStatement(FunctionStatement* node)
{
  FunctionSymbol* sym = node->sym();

  assert(!fs_);

  if (!fs_ && sym->shadows()) {
    // We are the root in a series of shadowed functions.
    analyzeShadowedFunctions(sym);
  }

  if (!node->body())
    return;

  // :TODO: forbid array returns?

  FuncState state(&fs_, node);
  visitBlockStatement(node->body());

  if (fs_->return_status == ReturnStatus::All)
    node->set_guaranteed_return();

  assert(fs_->return_status != ReturnStatus::Mixed);
  global_functions_.push_back(node);
}

// :TODO: write tests for this.
void
SemanticAnalysis::analyzeShadowedFunctions(FunctionSymbol* sym)
{
  // We do not yet support overloading, so two functions with the same name
  // and a body are illegal. We consider natives to be implemented.
  FunctionStatement* impl = nullptr;

  // We support non-native implementations of a forwarded function.
  FunctionStatement* forward = nullptr;

  for (size_t i = 0; i < sym->shadows()->size(); i++) {
    FunctionStatement* stmt = sym->shadows()->at(i);
    switch (stmt->token()) {
      case TOK_FORWARD:
        if (forward) {
          cc_.report(stmt->loc(), rmsg::function_redeclared)
            << stmt->name()
            << (cc_.note(forward->loc(), rmsg::previous_location));
          continue;
        }
        forward = stmt;
        break;
      case TOK_NATIVE:
      case TOK_FUNCTION:
        if (impl) {
          cc_.report(stmt->loc(), rmsg::function_redeclared)
            << stmt->name()
            << (cc_.note(impl->loc(), rmsg::previous_location));
          continue;
        }
        impl = stmt;
        break;
      default:
        assert(false);
        break;
    }
  }

  // If we have both an impl and a forward, make sure they match.
  if (impl && forward)
    checkForwardedFunction(forward, impl);
}

void
SemanticAnalysis::checkForwardedFunction(FunctionStatement* forward, FunctionStatement* impl)
{
  // SP1 didn't check these. We tighten up the semantics a bit for SP2.
  if (impl->token() == TOK_NATIVE) {
    cc_.report(impl->loc(), rmsg::illegal_forward_native)
      << impl->name()
      << cc_.note(forward->loc(), rmsg::previous_location);
    return;
  }
   
  if (impl->token() != TOK_PUBLIC) {
    cc_.report(impl->loc(), rmsg::illegal_forward_func)
      << impl->name()
      << cc_.note(forward->loc(), rmsg::previous_location);
    return;
  }

  FunctionSignature* fwdSig = forward->signature();
  FunctionSignature* implSig = impl->signature();

  if (!matchForwardSignatures(fwdSig, implSig)) {
    cc_.report(impl->loc(), rmsg::forward_signature_mismatch)
      << impl->name()
      << cc_.note(forward->loc(), rmsg::previous_location);
    return;
  }
}

bool
SemanticAnalysis::matchForwardSignatures(FunctionSignature* fwdSig, FunctionSignature* implSig)
{
  // Due to SourceMod oddness, and the implementation detail that arguments are
  // pushed in reverse order, the impl function is allowed to leave off any
  // number of arguments. But, it cannot have more arguments.
  if (fwdSig->parameters()->size() < implSig->parameters()->size())
    return false;

  // We allow return types to differ iff the forward's type is void and the
  // impl function is implicit-int.
  Type* fwdRetType = fwdSig->returnType().resolved();
  Type* implRetType = implSig->returnType().resolved();
  if (!matchForwardReturnTypes(fwdRetType, implRetType))
    return false;

  return true;
}

bool
SemanticAnalysis::matchForwardReturnTypes(Type* fwdRetType, Type* implRetType)
{
  if (AreTypesEquivalent(fwdRetType, implRetType, Qualifiers::None))
    return true;
  if ((fwdRetType->isVoid() || fwdRetType->isImplicitInt()) && implRetType->isImplicitVoid())
    return true;
  return false;
}

void
SemanticAnalysis::visitBlockStatement(BlockStatement* node)
{
  for (size_t i = 0; i < node->statements()->size(); i++) {
    Statement* ast_stmt = node->statements()->at(i);
    visitStatement(ast_stmt);
  }
}

void
SemanticAnalysis::visitStatement(Statement* node)
{
  switch (node->kind()) {
    case AstKind::kReturnStatement:
      visitReturnStatement(node->toReturnStatement());
      break;
    case AstKind::kExpressionStatement:
      visitExpressionStatement(node->toExpressionStatement());
      break;
    case AstKind::kVarDecl:
    {
      for (VarDecl* decl = node->toVarDecl(); decl; decl = decl->next())
        visitVarDecl(decl);
      break;
    }
    case AstKind::kWhileStatement:
      visitWhileStatement(node->toWhileStatement());
      break;
    case AstKind::kForStatement:
      visitForStatement(node->toForStatement());
      break;
    case AstKind::kBlockStatement:
      visitBlockStatement(node->toBlockStatement());
      break;
    case AstKind::kIfStatement:
      visitIfStatement(node->toIfStatement());
      break;
    case AstKind::kBreakStatement:
      visitBreakStatement(node->toBreakStatement());
      break;
    case AstKind::kContinueStatement:
      visitContinueStatement(node->toContinueStatement());
      break;
    case AstKind::kSwitchStatement:
      visitSwitchStatement(node->toSwitchStatement());
      break;
    default:
      cc_.report(node->loc(), rmsg::unimpl_kind) <<
        "sema-visit-stmt" << node->kindName();
      break;
  }
}

void
SemanticAnalysis::visitVarDecl(VarDecl* node)
{
  VariableSymbol* sym = node->sym();
  assert(node->classifier() == TOK_NEW || node->classifier() == TOK_PUBLIC);

  // :TODO: unused var analysis
  // :TODO: multiple in chain

  sema::Expr* init = nullptr;
  if (node->initialization()) {
    if ((init = initializer(node->initialization(), sym->type())) == nullptr)
      return;

    if (sym->scope()->kind() == Scope::Global && !init->isConstant()) {
      cc_.report(node->loc(), rmsg::global_non_const_init) << sym->name();
      return;
    }

    node->set_sema_init(init);
  } else {
    if (ArrayType* type = node->type()->asArray()) {
      uint32_t dynamic_levels = 0;
      for (ArrayType* iter = type; iter; iter = iter->contained()->asArray()) {
        if (!iter->hasFixedLength())
          dynamic_levels++;
      }

      // There is currently no way to mix/match fixed and dynamic length
      // arrays. We might get this someday via typedefs, but not today.
      assert(!dynamic_levels || dynamic_levels == type->nlevels());

      if (dynamic_levels) {
        // We should have created an initializer during type resolution. If we
        // didn't, something extremely weird happened, or the declaration is
        // just invalid. Like "new int[]" or something.
        if (type->isCharArray())
          cc_.report(node->loc(), rmsg::dynamic_array_needs_new_or_str);
        else
          cc_.report(node->loc(), rmsg::dynamic_array_needs_new);
        return;
      }
    }
  }

  if (!node->must_zero_init()) {
    // :TODO: forbid static/global
    if (!sym->type()->isArray()) {
      cc_.report(node->loc(), rmsg::decl_needs_array_type);
      return;
    }

    ArrayType* type = sym->type()->toArray();
    if (type->hasFixedLength() && node->initialization()) {
      cc_.report(node->initialization()->loc(), rmsg::decl_with_initializer) <<
        sym->type();
      return;
    }
    if (type->contained()->isConst()) {
      cc_.report(node->loc(), rmsg::decl_with_const_array);
      return;
    }

    assert(sym->scope()->kind() == Scope::Block);
  }

  if (sym->scope()->kind() == Scope::Global)
    global_vars_.push_back(node);
}

void
SemanticAnalysis::visitWhileStatement(WhileStatement* node)
{
  ke::SaveAndSet<size_t> enter_loop(&loop_depth_, loop_depth_ + 1);

  // :TODO: implement unintended-assignment warning

  // Even if we can't coerce the stop expression, we still type-check the body.
  TestEvalContext ec(cc_, node->condition());
  if (coerce(ec))
    node->set_sema_cond(ec.result);

  // :TODO: check for infinite loop, if no breaks.
  visitStatement(node->body());
}

void
SemanticAnalysis::visitForStatement(ForStatement* node)
{
  // :TODO: infinite loop check? see if sp1 does

  if (Statement* init = node->initialization())
    visitStatement(init);
  if (Expression* cond = node->condition()) {
    TestEvalContext ec(cc_, cond);
    if (coerce(ec))
      node->set_sema_cond(ec.result);
  }

  // Break/continue are only valid inside the body.
  {
    ke::SaveAndSet<size_t> enter_loop(&loop_depth_, loop_depth_ + 1);
    visitStatement(node->body());
  }

  if (Statement* update = node->update())
    visitStatement(update);
}

void
SemanticAnalysis::visitIfStatement(IfStatement* node)
{
  for (size_t i = 0; i < node->clauses()->size(); i++) {
    IfClause& clause = node->clauses()->at(i);

    TestEvalContext ec(cc_, clause.cond);
    if (coerce(ec))
      clause.sema_cond = ec.result;

    visitStatement(clause.body);
  }

  if (Statement* body = node->fallthrough())
    visitStatement(body);
}

void
SemanticAnalysis::visitReturnStatement(ReturnStatement* node)
{
  FunctionSignature* sig = fs_->sig;
  Type* returnType = sig->returnType().resolved();

  fs_->return_status = ReturnStatus::All;

  // :TODO: unreachable code warning

  if (returnType->isVoid()) {
    if (node->expr())
      cc_.report(node->loc(), rmsg::returned_in_void_function);
    return;
  }

  if (!node->expr()) {
    // This is okay for simple types.
    if (returnType->isPrimitive() || returnType->isEnum())
      return;

    // Not okay for more advanced types.
    cc_.report(node->loc(), rmsg::need_return_value);
    return;
  }

  EvalContext ec(CoercionKind::Return, node->expr(), returnType);
  if (coerce(ec))
    node->set_sema_expr(ec.result);
}

void
SemanticAnalysis::visitBreakStatement(BreakStatement* node)
{
  if (!loop_depth_) {
    cc_.report(node->loc(), rmsg::break_outside_loop);
    return;
  }
}

void
SemanticAnalysis::visitContinueStatement(ContinueStatement* node)
{
  if (!loop_depth_) {
    cc_.report(node->loc(), rmsg::continue_outside_loop);
    return;
  }
}

void
SemanticAnalysis::visitSwitchStatement(SwitchStatement* node)
{
  Type* type = nullptr;

  sema::Expr* expr = visitExpression(node->expression());
  if (expr) {
    EvalContext ec(CoercionKind::RValue, expr, expr->type());
    if (coerce(ec)) {
      node->set_sema_expr(ec.result);
      type = expr->type();
    }
  }

  auto get_value = [&, this](Expression* case_expr) -> int32_t {
    if (!type)
      return 0;

    EvalContext ec(CoercionKind::Expr, case_expr, type);
    if (!coerce(ec))
      return 0;

    BoxedValue box;
    if (!ec.from->getBoxedValue(&box) || !box.isInteger()) {
      cc_.report(case_expr->loc(), rmsg::illegal_case);
      return 0;
    }

    const IntValue& ival = box.toInteger();
    if (ival.numBits() > (sizeof(int32_t) * 8)) {
      cc_.report(case_expr->loc(), rmsg::illegal_64bit_case);
      return 0;
    }

    return ival.asSigned();
  };

  PoolList<ast::Case*>* cases = node->cases();
  for (size_t i = 0; i < cases->size(); i++) {
    ast::Case* entry = cases->at(i);

    size_t ncases = 1 + (entry->others()
                         ? entry->others()->size()
                         : 0);
    FixedPoolList<int32_t>* values = new (pool_) FixedPoolList<int32_t>(ncases);

    values->at(0) = get_value(entry->expression());
    if (const auto& others = entry->others()) {
      for (size_t j = 0; j < others->size(); j++)
        values->at(j + 1) = get_value(others->at(j));
    }
    entry->setValues(values);

    visitStatement(entry->statement());

    // :TODO: detect duplicates?
  }

  if (Statement* stmt = node->defaultCase())
    visitStatement(stmt);
}

void
SemanticAnalysis::visitExpressionStatement(ExpressionStatement* node)
{
  sema::Expr* expr = visitExpression(node->expr());
  if (!expr)
    return;

  // We really want to peel away lvalues here...
  EvalContext ec(CoercionKind::RValue, expr, expr->type());
  if (!coerce(ec))
    return;
  expr = ec.result;

  if (!expr->hasSideEffects())
    cc_.report(node->loc(), rmsg::statement_has_no_effect);

  node->set_sema_expr(expr);

  // :TODO: eliminate load-after-stores, for example, i++.
  // ^-- this comment should be in smx-compiler
}

} // namespace sp
