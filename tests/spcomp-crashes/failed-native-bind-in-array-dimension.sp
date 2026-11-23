// Regression test for a compiler crash (issue 1228).
//
// A native whose return type cannot be bound leaves its signature null.
// Calling it from an array dimension expression, which is evaluated early,
// used to dereference that null signature. Compilation must fail, but must
// not crash.
native u n();
f() { int a[n()]; }
