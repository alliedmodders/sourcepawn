// Regression test for a compiler crash (issue 1228).
//
// The dimension of an old-style array is evaluated before a 'let'
// variable has its type inferred. Using such a variable as a dimension
// used to trip an assertion in the type system. Compilation must fail,
// but must not crash.
f() { let y = 1; new x[y]; }
