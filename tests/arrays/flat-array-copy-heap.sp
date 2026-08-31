#include <shell>

// Regression test: copying flat arrays of heap items must not crash.
// This exercises LL_COPYARRAY_FLAT_A in the interpreter.

class C {
    int x;
}

public main() {
    // Two flat arrays of objects. Elements are null since we cannot yet
    // store heap items into flat array elements, but the copy path must
    // still handle the refcount loop without crashing.
    C a[3];
    C b[3];
    a = b;
    print("copy ok\n");

    // Self-copy must also not crash.
    a = a;
    print("self-copy ok\n");
}
