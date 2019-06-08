// vim: set sts=2 ts=8 sw=2 tw=99 et:
// 
// CopyaRight (C) 2012-2014 David Anderson
// 
// This file is part of SourcePawn.
// 
// SourcePawn is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option)
// any later version.
// 
// SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
#include "float-value.h"
#include <amtl/am-float.h>

using namespace ke;
using namespace sp;

bool
FloatValue::UpcastForOp(FloatValue* aLeft, FloatValue* aRight)
{
  if (aLeft->isDouble() != aRight->isDouble()) {
    *aLeft = FromDouble(aLeft->asDouble());
    *aRight = FromDouble(aRight->asDouble());
    return true;
  }
  return aLeft->isDouble();
}

FloatValue 
FloatValue::Add(const FloatValue& aLeft, const FloatValue& aRight)
{
  FloatValue left = aLeft, right = aRight;
  if (UpcastForOp(&left, &right))
    return FromDouble(left.toDouble() + right.toDouble());
  return FromFloat(left.toFloat() + right.toFloat());
}

FloatValue 
FloatValue::Sub(const FloatValue& aLeft, const FloatValue& aRight)
{
  FloatValue left = aLeft, right = aRight;
  if (UpcastForOp(&left, &right))
    return FromDouble(left.toDouble() - right.toDouble());
  return FromFloat(left.toFloat() - right.toFloat());
}

FloatValue
FloatValue::Mul(const FloatValue& aLeft, const FloatValue& aRight)
{
  FloatValue left = aLeft, right = aRight;
  if (UpcastForOp(&left, &right))
    return FromDouble(left.toDouble() * right.toDouble());
  return FromFloat(left.toFloat() * right.toFloat());
}

FloatValue
FloatValue::Div(const FloatValue& aLeft, const FloatValue& aRight)
{
  FloatValue left = aLeft, right = aRight;
  if (UpcastForOp(&left, &right))
    return FromDouble(left.toDouble() / right.toDouble());
  return FromFloat(left.toFloat() / right.toFloat());
}

FloatValue
FloatValue::Mod(const FloatValue& aLeft, const FloatValue& aRight)
{
  FloatValue left = aLeft, right = aRight;
  if (UpcastForOp(&left, &right))
    return FromDouble(FloatModulo(left.toDouble(), right.toDouble()));
  return FromFloat(FloatModulo(left.toFloat(), right.toFloat()));
}

bool
FloatValue::Ge(const FloatValue& aLeft, const FloatValue& aRight)
{
  FloatValue left = aLeft, right = aRight;
  if (UpcastForOp(&left, &right))
    return left.toDouble() >= right.toDouble();
  return left.toFloat() >= right.toFloat();
}

bool
FloatValue::Gt(const FloatValue& aLeft, const FloatValue& aRight)
{
  FloatValue left = aLeft, right = aRight;
  if (UpcastForOp(&left, &right))
    return left.toDouble() > right.toDouble();
  return left.toFloat() > right.toFloat();
}

bool
FloatValue::Le(const FloatValue& aLeft, const FloatValue& aRight)
{
  FloatValue left = aLeft, right = aRight;
  if (UpcastForOp(&left, &right))
    return left.toDouble() <= right.toDouble();
  return left.toFloat() <= right.toFloat();
}

bool
FloatValue::Lt(const FloatValue& aLeft, const FloatValue& aRight)
{
  FloatValue left = aLeft, right = aRight;
  if (UpcastForOp(&left, &right))
    return left.toDouble() < right.toDouble();
  return left.toFloat() < right.toFloat();
}

bool
FloatValue::Eq(const FloatValue& aLeft, const FloatValue& aRight)
{
  FloatValue left = aLeft, right = aRight;
  if (UpcastForOp(&left, &right))
    return left.toDouble() == right.toDouble();
  return left.toFloat() == right.toFloat();
}

bool
FloatValue::Ne(const FloatValue& aLeft, const FloatValue& aRight)
{
  FloatValue left = aLeft, right = aRight;
  if (UpcastForOp(&left, &right))
    return left.toDouble() != right.toDouble();
  return left.toFloat() != right.toFloat();
}

FloatValue
FloatValue::Neg(const FloatValue& in)
{
  if (in.isDouble())
    return FromDouble(-in.toDouble());
  return FromFloat(-in.toFloat());
}
