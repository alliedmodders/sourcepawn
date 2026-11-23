// Regression test for a compiler crash (issue 1228).
//
// A function name was used as part of the function's own signature, via an
// argument dimension. The signature is cyclic and cannot be resolved;
// compilation must fail, but must not crash.
f(a[f]) {}
