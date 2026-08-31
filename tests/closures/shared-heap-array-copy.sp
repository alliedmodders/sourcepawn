#include <shell>

class Player {
    int id;
}

typedef Callback = () -> int;

// Verify that shared fixed-size arrays of heap items (a "flat-seeming" array
// whose element is a class type, so it is internally non-flat) are assigned
// by copy. OP_COPYARRAY emits LL_COPYARRAY_A, which copies the array slot
// contents with refcount bumping. The two arrays are slot-independent, but
// the inner Player objects are shared.
Callback GetSharedHeapArrayCopyCallback() {
    shared Player a[3];
    shared Player b[3];

    a[0] = new Player(); a[0].id = 10;
    a[1] = new Player(); a[1].id = 20;
    a[2] = new Player(); a[2].id = 30;

    b = a; // OP_COPYARRAY: cell-by-cell with refcount bump

    // Replace a[0] with a fresh Player; b[0] keeps the original slot.
    a[0] = new Player(); a[0].id = 999;

    // Mutating a[1].id is visible via b[1] because the inner Player is shared.
    a[1].id = 200;

    // Reference both arrays so both count as captured (error 482).
    // The assertion uses b-side values to verify the copy semantics:
    //   b[0] = original (10)  -> slot-independent
    //   b[1] = shared mut'd (200) -> shared Player
    //   b[2] = original (30)  -> untouched
    return function () -> int {
        return a[2].id + b[0].id + b[1].id + b[2].id;
    };
}

public void main() {
    printnum(GetSharedHeapArrayCopyCallback()()); // expects 30 + 10 + 200 + 30 = 270
}
