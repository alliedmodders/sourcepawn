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

#include <amtl/am-enum.h>
#include <amtl/am-flags.h>
#include "boxed-value.h"

namespace sp {

enum class SymAttrs
{
  None          = 0x0,
  Stock         = 0x1,
  Uninitialized = 0x2
};
KE_DEFINE_ENUM_OPERATORS(SymAttrs)

// Storage class for variables.
enum class StorageClass : int32_t
{
  Unknown,
  Local,
  Argument,
  Global
};

} // namespace sp

#endif // _include_spcomp_expression_ops_h_
