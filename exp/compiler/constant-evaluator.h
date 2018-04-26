/* vim: set ts=8 sts=2 sw=2 tw=99 et:
 *
 * Copyright (C) 2012-2014 David Anderson
 *
 * This file is part of SourcePawn.
 *
 * SourcePawn is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 * 
 * SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * SourcePawn. If not, see http://www.gnu.org/licenses/.
 */
#ifndef _include_sp2_constant_evaluator_h_
#define _include_sp2_constant_evaluator_h_

#include "types.h"
#include "symbols.h"

namespace sp {

class Scope;
class CompileContext;

namespace ast {
class Expression;
class UnaryExpression;
class BinaryExpression;
} // namespace ast

// We generally don't care about the ordering of constants in global scope, so
// to make sure we resolve things like:
//     int x[B];
//     enum { B = 5 };
// We allow resolution to recursively re-enter the type resolver.
class ConstantResolver
{
 public:
  virtual Type* resolveTypeOfVar(VariableSymbol* sym) {
    return sym->type();
  }
  virtual bool resolveVarAsConstant(VariableSymbol* sym, BoxedValue* out) {
    if (!sym->isConstExpr())
      return false;
    *out = sym->constExpr();
    return true;
  }
  virtual BoxedValue resolveValueOfConstant(ConstantSymbol* sym) {
    return sym->value();
  }
};

class ConstantEvaluator
{
 public:
  enum Result {
    // A constant was evaluated.
    Ok,

    // A type error was reported.
    TypeError,

    // The expression is not constant.
    NotConstant,

    // The expression could not be evaluated because it relies on a symbol
    // that has not yet been evaluated.
    Pending
  };
  
  enum Mode {
    Speculative,
    Required
  };

  ConstantEvaluator(CompileContext& cc, ConstantResolver* resolver, Mode mode)
   : cc_(cc),
     resolver_(resolver),
     mode_(mode)
  {
    if (!resolver_)
      resolver_ = &default_resolver_;
  }

  Result Evaluate(ast::Expression* expr, BoxedValue* out);

 private:
  Result unary(ast::UnaryExpression* expr, BoxedValue& inner, BoxedValue* out);
  Result binary(ast::BinaryExpression* expr, BoxedValue& left, BoxedValue& right, BoxedValue* out);

 private:
  CompileContext& cc_;
  ConstantResolver* resolver_;
  ConstantResolver default_resolver_;
  Mode mode_;
};

}

#endif // _include_sp2_constant_evaluator_h_
