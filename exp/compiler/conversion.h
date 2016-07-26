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
#include "reporting.h"

namespace sp {

enum class ConversionKind : int32_t
{
  // No possible conversion.
  error = 0,

  // No conversion necessary.
  none,

  // Trivial const conversion necessary.
  trivial_const,

  // Downcast from a derived to a base type.
  downcast,

  // Store a null as a 32-bit integer value.
  null_to_int,

  // Store a null as a function or object value.
  null_to_obj,

  // Bitwise representation of a type X to Y (types must be the same width).
  bitcast,

  // Convert an 8, 16, or 32-bit integer equivalent to an unchecked value.
  // This is a bitcast, preceded by a sign or zero extension if necessary.
  to_unchecked,

  // Convert a value to a bool.
  to_bool,

  // Promote a float to a double.
  float_to_double,

  // Convert an integer to a float or double.
  int_to_float,

  // An assignment from char[X+n] to char[X+n] where m < n.
  char_array_extend,

  // Sign or zero-extend an integer to an integer of a greater width.
  promote_int,

  // Coercing T[N] -> T[]
  fixed_array_decay,

  sentinel
};

enum class ConversionError
{
  none,
  discards_const_qualifiers,
  loses_precision,
  ambiguous,
  ref_is_readonly,
  ref_expr_not_lvalue,
  type_mismatch,
  sentinel
};

struct ConversionResult
{
  ConversionResult(ConversionKind kind, Qualifiers quals = Qualifiers::None)
    : kind(kind),
      error(ConversionError::none),
      trivialConst((quals & Qualifiers::Const) == Qualifiers::Const),
      deref(false)
  {}
  ConversionResult(ConversionError error)
    : kind(ConversionKind::error),
      error(error)
  {}
  MessageBuilder diag(CompileContext &cc, const SourceLocation &loc);
  ConversionKind kind;
  ConversionError error;

  // This conversion involves adding a final "trivial const", for example,
  //  int[] -> const int[].
  bool trivialConst : 1;

  // This conversion requires an initial step adding a dereference operation.
  bool deref : 1;
};

class Conversion
{
public:
  static ConversionResult Find(Type *from, Type *to);

private:
  static ConversionResult CoerceToEnum(Type *from, Type *to);
  static ConversionResult CoerceToUnchecked(Type *from, Type *to);
  static ConversionResult CoerceToBool(Type *from, Type *to);
  static ConversionResult CoerceToFloat(Type *from, Type *to);
  static ConversionResult CoerceToInt(Type *from, Type *to);
  static ConversionResult CoerceToFixedArray(Type *aStart, Type *aDest);
  static ConversionResult CoerceToArray(Type *aStart, Type *aDest);

  static bool IsCompatibleWithUnchecked(Type *t);
};

} // namespace sp

#endif // _include_spcomp_coercion_h_
