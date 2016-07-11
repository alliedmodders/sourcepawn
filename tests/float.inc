#if defined _float_included
 #endinput
#endif
#define _float_included

#if !defined __sourcepawn2__
/**
 * Converts an integer into a floating point value.
 *
 * @param value			Integer to convert.
 * @return				Floating point value.
 */
native float float(int value);
#endif

native float FloatMul(float oper1, float oper2);
native float FloatDiv(float dividend, float divisor);
native float FloatAdd(float oper1, float oper2);
native float FloatSub(float oper1, float oper2);
native int RoundToZero(float value);
native int RoundToCeil(float value);
native int RoundToFloor(float value);
native int RoundToNearest(float value);
native int FloatCompare(float fOne, float fTwo);
native float FloatAbs(float value);

#if !defined __sourcepawn2__
#pragma rational Float

native bool __FLOAT_GT__(float a, float b);
native bool __FLOAT_GE__(float a, float b);
native bool __FLOAT_LT__(float a, float b);
native bool __FLOAT_LE__(float a, float b);
native bool __FLOAT_EQ__(float a, float b);
native bool __FLOAT_NE__(float a, float b);
native bool __FLOAT_NOT__(float a);

native float operator*(float oper1, float oper2) = FloatMul;
native float operator/(float oper1, float oper2) = FloatDiv;
native float operator+(float oper1, float oper2) = FloatAdd;
native float operator-(float oper1, float oper2) = FloatSub;
native bool operator!(float oper1) = __FLOAT_NOT__;
native bool operator>(float oper1, float oper2) = __FLOAT_GT__;
native bool operator>=(float oper1, float oper2) = __FLOAT_GE__;
native bool operator<(float oper1, float oper2) = __FLOAT_LT__;
native bool operator<=(float oper1, float oper2) = __FLOAT_LE__;
native bool operator!=(float oper1, float oper2) = __FLOAT_NE__;
native bool operator==(float oper1, float oper2) = __FLOAT_EQ__;
#endif // __sourcepawn2__
