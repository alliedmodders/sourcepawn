// vim: set sts=2 ts=8 sw=2 tw=99 et:
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
#ifndef _include_spcomp_coercion_h_
#define _include_spcomp_coercion_h_

#include "ast.h"
#include <am-utility.h>

namespace sp {

// Handle implicit coercion rules.
class Coercion
{
 public:
  enum class Reason
  {
    arg,
    assign,
    shallow
  };

  enum class Result {
    // Success.
    ok,

    // Almost-success conditions.
    discards_const_qualifiers,
    loses_precision,
    ambiguous,
    ref_is_readonly,
    ref_expr_not_lvalue,

    // Failure conditions.
    type_mismatch,

    sentinel
  };

 public:
  Coercion(CompileContext &cc, Reason reason, Expression *expr, Type *to);

  Result coerce();
  Expression *output() const {
    return out_;
  }
  PassRef<TMessage> message() const {
    return message_;
  }

  // Maybe add a diagnostic note for a failed coercion.
  MessageBuilder diag(const SourceLocation &loc);

 private:
  Result coerceToRef();
  Result coerceInner();
  Result coerceToEnum();
  Result coerceToUnchecked();
  Result coerceToBool();
  Result coerceToFloat();
  Result coerceToInt();
  Result coerceToFixedArray();
  Result coerceToTypeset();
  Result coerceToArray();
  Result maybeCoerceToConst();

 private:
  CompileContext &cc_;
  PoolAllocator &pool_;
  Reason reason_;
  Expression *expr_;
  Type *to_;
  Expression *out_;
  Result result_;
  Ref<TMessage> message_;
};

} // namespace sp

#endif // _include_spcomp_coercion_h_
