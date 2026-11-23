// Regression test for a compiler crash (issue 1228).
//
// Old-style array dimensions are evaluated before function signatures are
// bound. Using a function as a dimension used to dereference a null
// signature. Compilation must fail, but must not crash.
f() {}
a[f]
