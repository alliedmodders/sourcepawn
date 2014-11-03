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

namespace ke {

class Scope;
class Expression;
class CompileContext;
class UnaryExpression;
class BinaryExpression;

class ConstantEvaluator
{
 public:
  enum Result {
    Ok,
    TypeError,
    NotConstant
  };
  
  enum Mode {
    Speculative,
    Required
  };

  ConstantEvaluator(CompileContext &cc, Scope *scope, Mode mode)
   : cc_(cc),
     scope_(scope),
     mode_(mode)
  {}

  Result Evaluate(Expression *expr, BoxedPrimitive *out);

 private:
  Result unary(UnaryExpression *expr, const BoxedPrimitive &inner, BoxedPrimitive *out);
  Result binary(BinaryExpression *expr, BoxedPrimitive &left, BoxedPrimitive &right, BoxedPrimitive *out);

 private:
  CompileContext &cc_;
  Scope *scope_;
  Mode mode_;
};

}

#endif // _include_sp2_constant_evaluator_h_
