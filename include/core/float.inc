/**
 * vim: set ts=4 :
 * =============================================================================
 * SourceMod (C)2004-2008 AlliedModders LLC.  All rights reserved.
 * =============================================================================
 *
 * This file is part of the SourceMod/SourcePawn SDK.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License, version 3.0, as published by the
 * Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITneSS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * As a special exception, AlliedModders LLC gives you permission to link the
 * code of this program (as well as its derivative works) to "Half-Life 2," the
 * "Source Engine," the "SourcePawn JIT," and any Game MODs that run on software
 * by the Valve Corporation.  You must obey the GNU General Public License in
 * all respects for all other code used.  Additionally, AlliedModders LLC grants
 * this exception to all derivative works.  AlliedModders LLC defines further
 * exceptions, found in LICENSE.txt (as of this writing, version JULY-31-2007),
 * or <http://www.sourcemod.net/license.php>.
 *
 * Version: $Id$
 */
 
#if defined _core_float_included
 #endinput
#endif
#define _core_float_included

#if !defined __sourcepawn2__
native float __float_ctor(int value);
native float __float_add(float a, float b);
native float __float_sub(float a, float b);
native float __float_mul(float a, float b);
native float __float_div(float dividend, float divisor);
native float __float_mod(float dividend, float divisor);
native bool __float_gt(float a, float b);
native bool __float_ge(float a, float b);
native bool __float_lt(float a, float b);
native bool __float_le(float a, float b);
native bool __float_eq(float a, float b);
native bool __float_ne(float a, float b);
native bool __float_not(float a);
native int __float_to_int(float a);
native float float(int value);

native float operator*(float oper1, float oper2) = __float_mul;
native float operator/(float oper1, float oper2) = __float_div;
native float operator+(float oper1, float oper2) = __float_add;
native float operator-(float oper1, float oper2) = __float_sub;
native float operator%(float oper1, float oper2) = __float_mod;
native bool operator!(float oper1) = __float_not;
native bool operator>(float oper1, float oper2) = __float_gt;
native bool operator>=(float oper1, float oper2) = __float_ge;
native bool operator<(float oper1, float oper2) = __float_lt;
native bool operator<=(float oper1, float oper2) = __float_le;
native bool operator!=(float oper1, float oper2) = __float_ne;
native bool operator==(float oper1, float oper2) = __float_eq;

stock float operator++(float oper)
{
  return oper+1.0;
}

stock float operator--(float oper)
{
  return oper-1.0;
}

stock float operator-(float oper)
{
  return oper^view_as<float>(cellmin);          /* IEEE values are sign/magnitude */
}

stock float operator*(float oper1, oper2)
{
  return __float_mul(oper1, float(oper2)); /* "*" is commutative */
}

stock float operator/(float oper1, oper2)
{
  return __float_div(oper1, float(oper2));
}

stock float operator/(oper1, float oper2)
{
  return __float_div(float(oper1), oper2);
}

stock float operator+(float oper1, oper2)
{
  return __float_add(oper1, float(oper2));      /* "+" is commutative */
}

stock float operator-(float oper1, oper2)
{
  return __float_sub(oper1, float(oper2));
}

stock float operator-(oper1, float oper2)
{
  return __float_sub(float(oper1), oper2);
}

stock bool operator==(float oper1, oper2)
{
  return __float_eq(oper1, float(oper2));
}

stock bool operator!=(float oper1, oper2)
{
  return __float_ne(oper1, float(oper2));
}

stock bool operator>(float oper1, oper2)
{
  return __float_gt(oper1, float(oper2));
}

stock bool operator>(oper1, float oper2)
{
  return __float_gt(float(oper1), oper2);
}

stock bool operator>=(float oper1, oper2)
{
  return __float_ge(oper1, float(oper2));
}

stock bool operator>=(oper1, float oper2)
{
  return __float_ge(float(oper1), oper2);
}

stock bool operator<(float oper1, oper2)
{
  return __float_lt(oper1, float(oper2));
}

stock bool operator<(oper1, float oper2)
{
  return __float_lt(float(oper1), oper2);
}

stock bool operator<=(float oper1, oper2)
{
  return __float_le(oper1, float(oper2));
}

stock bool operator<=(oper1, float oper2)
{
  return __float_le(float(oper1), oper2);
}

stock float operator%(float oper1, oper2)
{
  return __float_mod(oper1, float(oper2));
}

stock float operator%(oper1, float oper2)
{
  return __float_mod(float(oper1), oper2);
}
#endif // __sourcepawn2__
