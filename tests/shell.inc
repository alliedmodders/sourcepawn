// vim: set ts=2 sw=2 tw=99 et:
#if defined _shell_included
 #endinput
#endif
#define _shell_included

#include <float>

native void printnum(int n);
native void printfloat(float n);
native void writefloat(float n);
native void printnums(any:...);
native void print(const char[] str);
native void dump_stack_trace();
native void unbound_native();
native int donothing();
