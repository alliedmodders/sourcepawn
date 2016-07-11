// vim: set ts=2 sw=2 tw=99 et:
#if defined _shell_included
 #endinput
#endif
#define _shell_included

native void printnum(int n);
native void printnums(any:...);
native void dump_stack_trace();
native void unbound_native();
native void print(const char[] str);
