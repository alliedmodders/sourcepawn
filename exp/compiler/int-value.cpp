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
#include <algorithm>

#include "int-value.h"
#include "compile-context.h"
#include "parser/tokens.h"

using namespace ke;
using namespace sp;

const char*
IntValue::getTypename() const
{
  switch (numBits()) {
    case 8:
      return isSigned() ? "int8" : "uint8";
    case 16:
      return isSigned() ? "int16" : "uint16";
    case 32:
      return isSigned() ? "int32" : "uint32";
    case 64:
      return isSigned() ? "int64" : "uint64";
    default:
      assert(false);
      return 0;
  }
  return "unknown";
}

bool
IntValue::ValidateAluOp(ReportingContext& cc, TokenKind kind, IntValue* aLeft, IntValue* aRight, bool* aSign)
{
  if (aLeft->isSigned() == aRight->isSigned()) {
    *aSign = aLeft->isSigned();

    unsigned bits = std::max(aLeft->numBits(), aRight->numBits());
    aLeft->bits_ = bits;
    aRight->bits_ = bits;
    return true;
  }

  if ((aLeft->isMaxBits() && aLeft->isUnsigned()) ||
      (aRight->isMaxBits() && aRight->isUnsigned()))
  {
    // We have an unsigned 64-bit number, plus any signed number. This would
    // require sign-extending both to a 128-bit number, which we don't support.
    cc.report(rmsg::int_type_overflow)
      << TokenNames[kind]
      << aLeft->getTypename()
      << aRight->getTypename();
    return false;
  }

  unsigned bits = std::max(aLeft->numBits(), aRight->numBits()) * 2;
  assert(bits <= kMaxBits && IsPowerOfTwo(bits));

  *aSign = true;
  *aLeft = FromSigned(aLeft->asSigned(), bits);
  *aRight = FromSigned(aRight->asSigned(), bits);
  return true;
}

bool
IntValue::Add(ReportingContext& cc, const IntValue& aLeft, const IntValue& aRight, IntValue* outp)
{
  bool sign;
  IntValue left = aLeft;
  IntValue right = aRight;
  if (!ValidateAluOp(cc, TOK_PLUS, &left, &right, &sign))
    return false;

  if (sign) {
    int64_t leftv = left.asSigned();
    int64_t rightv = right.asSigned();
    if ((rightv > 0 && (leftv > MaxSigned(left.numBits()) - rightv)) ||
        (rightv < 0 && (leftv < MinSigned(left.numBits()) - rightv)))
    {
      cc.report(rmsg::constexpr_overflow) << left.getTypename();
      return false;
    }
    *outp = FromSigned(leftv + rightv, left.numBits());
  } else {
    uint64_t leftv = left.asUnsigned();
    uint64_t rightv = right.asUnsigned();
    uint64_t result = leftv + rightv;

    // Check both 64-bit overflow and locally bounded overflow.
    if ((result < std::max(leftv, rightv)) || Log2(result) > left.numBits()) {
      cc.report(rmsg::constexpr_overflow) << left.getTypename();
      return false;
    }
    *outp = FromUnsigned(result, left.numBits());
  }

  return true;
}

bool
IntValue::Sub(ReportingContext& cc, const IntValue& aLeft, const IntValue& aRight, IntValue* outp)
{
  bool sign;
  IntValue left = aLeft;
  IntValue right = aRight;
  if (!ValidateAluOp(cc, TOK_MINUS, &left, &right, &sign))
    return false;

  if (sign) {
    int64_t leftv = left.asSigned();
    int64_t rightv = right.asSigned();
    if ((rightv < 0 && leftv < (MinSigned(left.numBits()) - rightv)) ||
        (rightv > 0 && leftv > (MaxSigned(left.numBits()) - rightv)))
    {
      cc.report(rmsg::constexpr_overflow) << left.getTypename();
      return false;
    }
    *outp = FromSigned(leftv - rightv, left.numBits());
  } else {
    if (right.asUnsigned() > right.asUnsigned()) {
      cc.report(rmsg::constexpr_overflow) << left.getTypename();
      return false;
    }
    *outp = FromUnsigned(left.asUnsigned() - right.asUnsigned(), left.numBits());
  }
  return true;
}

bool
IntValue::Mul(ReportingContext& cc, const IntValue& aLeft, const IntValue& aRight, IntValue* outp)
{
  bool sign;
  IntValue left = aLeft;
  IntValue right = aRight;
  if (!ValidateAluOp(cc, TOK_STAR, &left, &right, &sign))
    return false;

  if (sign) {
    int64_t result = left.asSigned() * right.asSigned();

    // Check 64-bit overflow.
    if (result != 0 && ((result / left.asSigned()) != left.asSigned())) {
      cc.report(rmsg::constexpr_overflow) << left.getTypename();
      return true;
    }

    // Check locally bounded overflow.
    if (result > MaxSigned(left.numBits()) || result < MinSigned(left.numBits())) {
      cc.report(rmsg::constexpr_overflow) << left.getTypename();
      return true;
    }
    *outp = FromSigned(result, left.numBits());
  } else {
    uint64_t result = left.asUnsigned() * right.asUnsigned();

    // Check 64-bit overflow.
    if (result != 0 && ((result / left.asUnsigned()) != right.asUnsigned())) {
      cc.report(rmsg::constexpr_overflow) << left.getTypename();
      return true;
    }

    // Check locally bounded overflow.
    if (result > MaxUnsigned(left.numBits())) {
      cc.report(rmsg::constexpr_overflow) << left.getTypename();
      return true;
    }
    *outp = FromUnsigned(result, left.numBits());
  }
  return true;
}

bool
IntValue::Div(ReportingContext& cc, const IntValue& aLeft, const IntValue& aRight, IntValue* outp)
{
  bool sign;
  IntValue left = aLeft;
  IntValue right = aRight;
  if (!ValidateAluOp(cc, TOK_SLASH, &left, &right, &sign))
    return false;

  if (right.isZero()) {
    cc.report(rmsg::divide_by_zero);
    return false;
  }

  if (sign) {
    // -INT_MIN / -1 == overflow
    if (left.asSigned() == MinSigned(left.numBits()) && right.asSigned() == -1) {
      cc.report(rmsg::constexpr_overflow) << left.getTypename();
      return false;
    }

    *outp = FromSigned(left.asSigned() / right.asSigned(), left.numBits());
  } else {
    *outp = FromUnsigned(left.asUnsigned() / right.asUnsigned(), left.numBits());
  }
  return true;
}

bool
IntValue::Mod(ReportingContext& cc, const IntValue& aLeft, const IntValue& aRight, IntValue* outp)
{
  bool sign;
  IntValue left = aLeft;
  IntValue right = aRight;
  if (!ValidateAluOp(cc, TOK_SLASH, &left, &right, &sign))
    return false;

  if (right.isZero()) {
    cc.report(rmsg::divide_by_zero);
    return false;
  }

  if (sign) {
    // -INT_MIN / -1 == overflow
    if (left.asSigned() == MinSigned(left.numBits()) && right.asSigned() == -1) {
      cc.report(rmsg::constexpr_overflow) << left.getTypename();
      return false;
    }

    *outp = FromSigned(left.asSigned() % right.asSigned(), left.numBits());
  } else {
    *outp = FromUnsigned(left.asUnsigned() % right.asUnsigned(), left.numBits());
  }
  return true;
}

bool
IntValue::Shl(ReportingContext& cc, const IntValue& left, const IntValue& right, IntValue* outp)
{
  if (left.isSigned())
    *outp = FromSigned(left.asSigned() << right.asSigned(), left.numBits());
  else
    *outp = FromUnsigned(left.asUnsigned() << right.asSigned(), left.numBits());
  return true;
}

bool
IntValue::Shr(ReportingContext& cc, const IntValue& left, const IntValue& right, IntValue* outp)
{
  if (left.isSigned())
    *outp = FromSigned(left.asSigned() >> right.asSigned(), left.numBits());
  else
    *outp = FromUnsigned(left.asUnsigned() >> right.asSigned(), left.numBits());
  return true;
}

bool
IntValue::Ushr(ReportingContext& cc, const IntValue& left, const IntValue& right, IntValue* outp)
{
  // >>> does unsigned shift against either signed or unsigned. Signedness of
  // rhs shouldn't matter, but we keep the whole thing unsigned just to be sure.
  *outp = FromRaw(left.asUnsigned() >> right.asUnsigned(), left.numBits(), left.isSigned());
  return true;
}

bool
IntValue::Or(ReportingContext& cc, const IntValue& aLeft, const IntValue& aRight, IntValue* outp)
{
  bool sign;
  IntValue left = aLeft;
  IntValue right = aRight;
  if (!ValidateAluOp(cc, TOK_BITOR, &left, &right, &sign))
    return false;
  *outp = FromRaw(left.asUnsigned() | right.asUnsigned(), left.numBits(), sign);
  return true;
}

bool
IntValue::And(ReportingContext& cc, const IntValue& aLeft, const IntValue& aRight, IntValue* outp)
{
  bool sign;
  IntValue left = aLeft;
  IntValue right = aRight;
  if (!ValidateAluOp(cc, TOK_BITAND, &left, &right, &sign))
    return false;
  *outp = FromRaw(left.asUnsigned() & right.asUnsigned(), left.numBits(), sign);
  return true;
}

bool
IntValue::Xor(ReportingContext& cc, const IntValue& aLeft, const IntValue& aRight, IntValue* outp)
{
  bool sign;
  IntValue left = aLeft;
  IntValue right = aRight;
  if (!ValidateAluOp(cc, TOK_BITXOR, &left, &right, &sign))
    return false;
  *outp = FromRaw(left.asUnsigned() ^ right.asUnsigned(), left.numBits(), sign);
  return true;
}

bool
IntValue::Ge(ReportingContext& cc, const IntValue& aLeft, const IntValue& aRight, bool* outp)
{
  bool sign;
  IntValue left = aLeft;
  IntValue right = aRight;
  if (!ValidateAluOp(cc, TOK_GE, &left, &right, &sign))
    return false;

  if (sign)
    *outp = left.asSigned() >= right.asSigned();
  else
    *outp = left.asUnsigned() >= right.asUnsigned();
  return true;
}

bool
IntValue::Gt(ReportingContext& cc, const IntValue& aLeft, const IntValue& aRight, bool* outp)
{
  bool sign;
  IntValue left = aLeft;
  IntValue right = aRight;
  if (!ValidateAluOp(cc, TOK_GT, &left, &right, &sign))
    return false;

  if (sign)
    *outp = left.asSigned() > right.asSigned();
  else
    *outp = left.asUnsigned() > right.asUnsigned();
  return true;
}

bool
IntValue::Le(ReportingContext& cc, const IntValue& aLeft, const IntValue& aRight, bool* outp)
{
  bool sign;
  IntValue left = aLeft;
  IntValue right = aRight;
  if (!ValidateAluOp(cc, TOK_LE, &left, &right, &sign))
    return false;

  if (sign)
    *outp = left.asSigned() <= right.asSigned();
  else
    *outp = left.asUnsigned() <= right.asUnsigned();
  return true;
}

bool
IntValue::Lt(ReportingContext& cc, const IntValue& aLeft, const IntValue& aRight, bool* outp)
{
  bool sign;
  IntValue left = aLeft;
  IntValue right = aRight;
  if (!ValidateAluOp(cc, TOK_LT, &left, &right, &sign))
    return false;

  if (sign)
    *outp = left.asSigned() < right.asSigned();
  else
    *outp = left.asUnsigned() < right.asUnsigned();
  return true;
}

bool
IntValue::Eq(ReportingContext& cc, const IntValue& aLeft, const IntValue& aRight, bool* outp)
{
  bool sign;
  IntValue left = aLeft;
  IntValue right = aRight;
  if (!ValidateAluOp(cc, TOK_EQUALS, &left, &right, &sign))
    return false;
  *outp = left.asUnsigned() == right.asUnsigned();
  return true;
}

bool
IntValue::Ne(ReportingContext& cc, const IntValue& aLeft, const IntValue& aRight, bool* outp)
{
  bool sign;
  IntValue left = aLeft;
  IntValue right = aRight;
  if (!ValidateAluOp(cc, TOK_NOTEQUALS, &left, &right, &sign))
    return false;
  *outp = left.asUnsigned() != right.asUnsigned();
  return true;
}

bool
IntValue::Neg(ReportingContext& cc, const IntValue& in, IntValue* outp)
{
  if (in.isUnsigned() && in.numBits() == kMaxBits) {
    cc.report(rmsg::implicit_overflow)
      << TokenNames[TOK_NEGATE]
      << in.getTypename();
    return false;
  }

  IntValue tmp = in;
  if (tmp.isUnsigned()) {
    tmp = FromSigned(tmp.asSigned(), tmp.numBits() * 2);
    assert(IsPowerOfTwo(tmp.numBits()));
  }

  *outp = FromSigned(-tmp.asSigned(), tmp.numBits());
  return true;
}

IntValue
IntValue::Invert(const IntValue& in)
{
  if (in.isSigned())
    return FromSigned(~in.asSigned(), in.numBits());
  return FromUnsigned(~in.asUnsigned(), in.numBits());
}
