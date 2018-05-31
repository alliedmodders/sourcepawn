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
#ifndef _include_spcomp_int_value_h_
#define _include_spcomp_int_value_h_

#include "token-kind.h"
#include <stdint.h>
#include <limits.h>

namespace sp {

struct ReportingContext;

#define SIGNED_INT_TYPE_MAP(_)    \
  _(int8_t,   Int8,   1)          \
  _(uint8_t,  Uint8,  0)          \
  _(int16_t,  Int16,  1)          \
  _(uint16_t, Uint16, 0)          \
  _(int32_t,  Int32,  1)          \
  _(uint32_t, Uint32, 0)          \
  _(int64_t,  Int64,  1)          \
  _(uint64_t, Uint64, 0)

namespace impl {
  struct true_value { static const bool value = true; };
  struct false_value { static const bool value = false; };

  template <typename T> struct is_signed {};
  template <> struct is_signed<int8_t> : public true_value {};
  template <> struct is_signed<int16_t> : public true_value {};
  template <> struct is_signed<int32_t> : public true_value {};
  template <> struct is_signed<int64_t> : public true_value {};
  template <> struct is_signed<uint8_t> : public false_value {};
  template <> struct is_signed<uint16_t> : public false_value {};
  template <> struct is_signed<uint32_t> : public false_value {};
  template <> struct is_signed<uint64_t> : public false_value {};

  template <typename T> struct limits {};
  template <> struct limits<uint8_t> {
    static const uint8_t max_value = UCHAR_MAX;
    static const uint8_t min_value = 0;
  };
  template <> struct limits<uint16_t> {
    static const uint16_t max_value = USHRT_MAX;
    static const uint16_t min_value = 0;
  };
  template <> struct limits<uint32_t> {
    static const uint32_t max_value = UINT_MAX;
    static const uint32_t min_value = 0;
  };
  template <> struct limits<uint64_t> {
    static const uint64_t max_value = ULLONG_MAX;
    static const uint64_t min_value = 0;
  };
  template <> struct limits<int8_t> {
    static const int8_t max_value = SCHAR_MAX;
    static const int8_t min_value = SCHAR_MIN;
  };
  template <> struct limits<int16_t> {
    static const int16_t max_value = SHRT_MAX;
    static const int16_t min_value = SHRT_MIN;
  };
  template <> struct limits<int32_t> {
    static const int32_t max_value = INT_MAX;
    static const int32_t min_value = INT_MIN;
  };
  template <> struct limits<int64_t> {
    static const int64_t max_value = LLONG_MAX;
    static const int64_t min_value = LLONG_MIN;
  };
} // namespace impl

struct IntValue
{
  static const uint32_t kMaxBits = sizeof(uint64_t) * 8;

 public:
  static IntValue FromRaw(uint64_t value, uint32_t bits, bool sign) {
    if (sign)
      return FromSigned(value, bits);
    return FromUnsigned(value, bits);
  }
  static IntValue FromSigned(int64_t value, uint32_t bits) {
    IntValue v;
    v.setSigned(value, bits);
    return v;
  }
  static IntValue FromUnsigned(uint64_t value, uint32_t bits) {
    IntValue v;
    v.setUnsigned(value, bits);
    return v;
  }

  template <typename T>
  static IntValue FromValue(T value) {
    IntValue v;
    if (impl::is_signed<T>::value)
      v.setSigned(value, sizeof(T) * 8);
    else
      v.setUnsigned(value, sizeof(T) * 8);
    return v;
  }

  int64_t asSigned() const {
    return ivalue_;
  }
  uint64_t asUnsigned() const {
    return uvalue_;
  }
  bool isZero() const {
    return uvalue_ == 0;
  }
  bool isMaxBits() const {
    return numBits() == kMaxBits;
  }

  // :TODO: test.
  template <typename T> bool safeCast(T* out) {
    if (impl::is_signed<T>::value != isSigned() && numBits() >= sizeof(T) * 8)
      return false;

    *out = static_cast<T>(ivalue_);
    return true;
  }

  template <typename T, typename V>
  static inline bool SafeCast(T value, V* out) {
    if (value < impl::limits<V>::min_value || value > impl::limits<V>::max_value)
      return false;
    *out = static_cast<V>(value);
    return true;
  }

  uint32_t numBits() const {
    return bits_;
  }
  bool isSigned() const {
    return is_signed_ == 1;
  }
  bool isUnsigned() const {
    return is_signed_ == 0;
  }

  void setUnsigned(uint64_t value, uint32_t bits) {
    uvalue_ = value;
    bits_ = bits;
    is_signed_ = 0;
  }
  void setSigned(int64_t value, uint32_t bits) {
    ivalue_ = value;
    bits_ = bits;
    is_signed_ = 1;
  }

  const char* getTypename() const;

  static inline int64_t MaxSigned(unsigned bits) {
    return ((int64_t(1) << (bits - 1)) - 1);
  }
  static inline int64_t MinSigned(unsigned bits) {
    return ~((int64_t(1) << (bits - 1)) - 1);
  }
  static inline uint64_t MaxUnsigned(unsigned bits) {
    return (uint64_t(1) << bits) - 1;
  }

  bool typeFitsInInt32() const {
    if (numBits() > 32)
      return false;
    return numBits() < 32 || (numBits() == 32 && isSigned());
  }
  bool valueFitsInInt32() const {
    if (isSigned())
      return asSigned() >= INT_MIN && asSigned() <= INT_MAX;
    return asUnsigned() <= INT_MAX;
  }
  bool isNegativeOrZero() const {
    return isSigned()
           ? asSigned() <= 0
           : asUnsigned() == 0;
  }

#define _(tname, pname, is_signed)        \
  void set##pname(tname v) {              \
    if (is_signed)                        \
      ivalue_ = v;                        \
    else                                  \
      uvalue_ = v;                        \
    bits_ = sizeof(v) * 8;                \
    is_signed_ = is_signed;               \
  }                                       \
  static IntValue From##pname(tname v) {  \
    IntValue iv;                          \
    iv.set##pname(v);                     \
    return iv;                            \
  }                                       \
  tname as##pname() const {               \
    return (tname)ivalue_;                \
  }
  SIGNED_INT_TYPE_MAP(_)
#undef _

  static bool Add(ReportingContext& cc, const IntValue& left, const IntValue& right, IntValue* outp);
  static bool Sub(ReportingContext& cc, const IntValue& left, const IntValue& right, IntValue* outp);
  static bool Mul(ReportingContext& cc, const IntValue& left, const IntValue& right, IntValue* outp);
  static bool Div(ReportingContext& cc, const IntValue& left, const IntValue& right, IntValue* outp);
  static bool Mod(ReportingContext& cc, const IntValue& left, const IntValue& right, IntValue* outp);
  static bool Shl(ReportingContext& cc, const IntValue& left, const IntValue& right, IntValue* outp);
  static bool Shr(ReportingContext& cc, const IntValue& left, const IntValue& right, IntValue* outp);
  static bool Ushr(ReportingContext& cc, const IntValue& left, const IntValue& right, IntValue* outp);
  static bool Or(ReportingContext& cc, const IntValue& left, const IntValue& right, IntValue* outp);
  static bool And(ReportingContext& cc, const IntValue& left, const IntValue& right, IntValue* outp);
  static bool Xor(ReportingContext& cc, const IntValue& left, const IntValue& right, IntValue* outp);
  static bool Ge(ReportingContext& cc, const IntValue& left, const IntValue& right, bool* outp);
  static bool Gt(ReportingContext& cc, const IntValue& left, const IntValue& right, bool* outp);
  static bool Le(ReportingContext& cc, const IntValue& left, const IntValue& right, bool* outp);
  static bool Lt(ReportingContext& cc, const IntValue& left, const IntValue& right, bool* outp);
  static bool Eq(ReportingContext& cc, const IntValue& left, const IntValue& right, bool* outp);
  static bool Ne(ReportingContext& cc, const IntValue& left, const IntValue& right, bool* outp);
  static bool Neg(ReportingContext& cc, const IntValue& in, IntValue* outp);
  static IntValue Invert(const IntValue& in);

 private:
  static bool ValidateAluOp(ReportingContext& cc, TokenKind tok, IntValue* left, IntValue* right, bool* sign);

 private:
  // Note: bits implicitly stores the type. We do not right-size the bitcount
  // since this would lose type information.
  uint32_t bits_ : 31;
  uint32_t is_signed_ : 1;
  union {
    uint64_t uvalue_;
    int64_t ivalue_;
  };
};

#undef SIGNED_INT_TYPE_MAP

}

#endif // _include_spcomp_int_value_h_
