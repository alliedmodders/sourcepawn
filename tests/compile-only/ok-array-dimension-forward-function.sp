// Regression test for a compiler crash (issue 1228).
//
// The dimension of an old-style array is evaluated before functions later
// in the file have been bound. The compiler binds the callee on demand, so
// this must compile.
int h() { return 4; }

void f() {
    new x[h()];
}

public void main() {
    f();
}
