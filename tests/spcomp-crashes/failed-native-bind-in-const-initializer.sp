// Regression test for a compiler crash (issue 1228).
//
// Constant initializers are also evaluated during binding. Calling a
// native whose return type failed to bind used to dereference a null
// signature. Compilation must fail, but must not crash.
native u n();
const int c = n();
