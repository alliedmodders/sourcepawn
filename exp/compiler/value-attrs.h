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
#ifndef _include_spcomp_expression_ops_h_
#define _include_spcomp_expression_ops_h_

#include <am-utility.h>
#include "boxed-value.h"

namespace sp {

enum class CastOp
{
  // No-operation; used for casts that do not affect the result type. Usually
  // this implies a cast to const, or from char -> int8.
  nop,

  // Convert an l-value or cl-value to a reference.
  lval_to_ref,

  // Dereference a reference. This produces an rvalue.
  deref,

  // Store a null as an enum value (32-bit 0).
  null_to_enum,

  // Store a null as a function or object value.
  null_to_obj,

  // Bitwise reinterpret of type X to Y (types must be same width).
  bitcast,

  // Convert an enum or 8, 16, or 32-bit integer to unchecked. This is a
  // bitcast, possibly preceded by a sign or zero extension.
  to_unchecked,

  // Convert a function to a meta function.
  to_metafunction,

  // Convert an integer or enum to a bool.
  int_to_bool,

  // Convert an integer to a float or double.
  int_to_float,

  // Promote an integer to a larger type (sign or zero-extend).
  promote_int,

  // Convert a float or double to a bool.
  float_to_bool,

  // Convert a float to a double.
  float_to_double,

  // An assignment from char[X+n] from char[X+m], where m < n.
  char_array_extend,

  // Coercing T[N] -> T[]
  fixed_array_decay,

  sentinel
};

// Value kinds.
enum class VK : int32_t
{
  none,

  // An lvalue can be the left-hand side of an assignment. LValues in Pawn are:
  //   NameProxies
  //   FieldExprs
  //   IndexExprs
  lvalue,

  // An rvalue is anything that is not an lvalue.
  rvalue
};

// Storage class for variables.
enum class StorageClass : int32_t
{
  Unknown,
  Local,
  Argument,
  Global
};

// L-value attribute flags.
enum class StorageFlags : int32_t
{
  none,

  // This l-value is stored by-reference.
  byref
};
KE_DEFINE_ENUM_OPERATORS(StorageFlags)

} // namespace sp

#endif // _include_spcomp_expression_ops_h_
