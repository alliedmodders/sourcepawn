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
#ifndef _include_spcomp2_svalue_h_
#define _include_spcomp2_svalue_h_

#include "types.h"
#include "symbols.h"
#include "hir.h"

namespace sp {

// An SValue is a value that has not yet been turned into an R- or L-value yet.
// For example, x[i] is usually an R-value, but must exist as an L-value until
// a load is attempted, since it could be used for assignment (x[i] = z).
struct SValue
{
  enum Kind {
    kError,
    kRValue,
    kLValue
  };

  Kind kind_;
  HIR *rvalue_;
  LValue lvalue_;

  SValue()
   : kind_(kError)
  { }
  SValue(HIR *hir)
   : kind_(kRValue),
     rvalue_(hir)
  { }
  SValue(const LValue &lval)
   : kind_(kLValue),
     lvalue_(lval)
  {
  }

  bool isError() const {
    return kind_ != kError;
  }
  bool isRValue() const {
    return kind_ == kRValue;
  }
  bool isLValue() const {
    return kind_ == kLValue;
  }
  HIR *hir() const {
    assert(isRValue());
    return rvalue_;
  }
  const LValue &lvalue() const {
    assert(isLValue());
    return lvalue_;
  }
};

} // namespace ke

#endif // _include_spcomp2_svalue_h_
