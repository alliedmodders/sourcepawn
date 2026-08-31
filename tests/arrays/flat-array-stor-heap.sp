#include <shell>

// Regression test: storing heap items (objects) into flat array elements.
// The compiler accepts flat arrays of HeapItem types, but the runtime
// currently has no store opcode for them (OP_STOR_ELEM_A on a flat array
// base hits an assert in lowering.cpp). This test captures the expected
// behavior once that is implemented.

class Player {
    int id;
}

public void main() {
    Player arr[3];
    arr[0] = new Player();
    arr[1] = new Player();
    arr[2] = new Player();
    arr[0].id = 1;
    arr[1].id = 2;
    arr[2].id = 3;
    printnum(arr[0].id + arr[1].id + arr[2].id);
}