#if defined _shell_float_included
 #endinput
#endif
#define _shell_float_included

#include <core/float>

native int RoundToZero(float value);
native int RoundToCeil(float value);
native int RoundToFloor(float value);
native int RoundToNearest(float value);
native float FloatAbs(float value);
