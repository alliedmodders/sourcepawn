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
#include "symbols.h"
#include "scopes.h"

using namespace ke;

bool
VariableSymbol::canUseInConstExpr() const
{
  // We allow "const int x" as an argument, though it's a mostly useless
  // construct. We don't allow it as a constexpr since its constant value
  // is not statically known, even if initialized.
  if (isArgument())
    return false;

  // "const" is sort of overloaded like it is in C++, of course. But like C++
  // we special case "const" and enum/int types (float as well, for us) as
  // being constexpr-capable.
  //
  // For declarations if something is constexpr-capable, Pawn actually requires
  // it to be a constexpr, unlike C++. Still, we distinguish between the two
  // in our constant evaluator.
  if (!type()->isConst())
    return false;
  if (!type()->canBeUsedInConstExpr())
    return false;

  return true;
}

bool
VariableSymbol::isArgument() const
{
  return scope()->isFunction();
}
