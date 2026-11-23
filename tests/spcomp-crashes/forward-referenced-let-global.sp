// Regression test for a compiler crash (issue 1228).
//
// A function body references a global 'let' declared later in the file.
// Its type is not inferred until analysis reaches the declaration, so the
// reference is an error, but it must not assert or crash.
f() { x = 0; }
let x = 0;
