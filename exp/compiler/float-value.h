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
#ifndef _include_spcomp_float_value_h_
#define _include_spcomp_float_value_h_

#include <assert.h>
#include <stdint.h>

namespace sp {

#define FLOAT_TYPE_MAP(_)           \
  _(float,   Float)                 \
  _(double,  Double)

struct FloatValue
{
 public:
  static FloatValue FromFloat(float f) {
    FloatValue v;
    v.bits_ = 32;
    v.float_ = f;
    return v;
  }
  static FloatValue FromDouble(double d) {
    FloatValue v;
    v.bits_ = 64;
    v.double_ = d;
    return v;
  }

  uint32_t numBits() const {
    return bits_;
  }
  bool isFloat() const {
    return bits_ == 32;
  }
  bool isDouble() const {
    return bits_ == 64;
  }

  bool isZero() const {
    if (isDouble())
      return !toDouble();
    return !toFloat();
  }

  float toFloat() const {
    assert(isFloat());
    return float_;
  }
  double toDouble() const {
    assert(isDouble());
    return double_;
  }

  float asFloat() const {
    if (isFloat())
      return float_;
    return (float)double_;
  }
  double asDouble() const {
    if (isDouble())
      return double_;
    return float_;
  }

  const char* getTypename() const {
    return isFloat() ? "float" : "double";
  }

  static FloatValue Add(const FloatValue& left, const FloatValue& right);
  static FloatValue Sub(const FloatValue& left, const FloatValue& right);
  static FloatValue Mul(const FloatValue& left, const FloatValue& right);
  static FloatValue Div(const FloatValue& left, const FloatValue& right);
  static FloatValue Mod(const FloatValue& left, const FloatValue& right);
  static bool Ge(const FloatValue& left, const FloatValue& right);
  static bool Gt(const FloatValue& left, const FloatValue& right);
  static bool Le(const FloatValue& left, const FloatValue& right);
  static bool Lt(const FloatValue& left, const FloatValue& right);
  static bool Eq(const FloatValue& left, const FloatValue& right);
  static bool Ne(const FloatValue& left, const FloatValue& right);
  static FloatValue Neg(const FloatValue& in);

 private:
  static bool UpcastForOp(FloatValue* left, FloatValue* right);

 private:
  uint32_t bits_;
  union {
    float float_;
    double double_;
  };
};

}

#endif // _include_spcomp_float_value_h_
