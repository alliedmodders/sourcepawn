// returnCode: 1
#include <shell>

// Regression test: the erroring frame must not have its gcobj registers
// released during unwind, because the prologue stack-limit checks fire
// before those registers are initialized. The dynamic array local gives f()
// a gcobj register; the recursion exhausts the stack inside f()'s prologue.
public void f() {
    int[] d = new int[19];   // allocated, never used
    f();
}

public void main() { f(); }
