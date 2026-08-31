#include <shell>

// DEMONSTRATES a pre-existing VM bug: heap (fixed) arrays of HeapItem
// elements crash at scope exit.
//
// Intended output: 6
// Current actual behavior: exit 139 (SIGSEGV) during scope-exit finalization.
//
// This test captures the behavior that the "flat arrays of HeapItem types
// must become fixed arrays" plan relies on. It is currently a known-broken
// path in the VM (NewArray of an object element, storing objects into it, and
// releasing them at scope exit) and must be fixed before that plan is viable.

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