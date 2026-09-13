#include <shell>

class Box {
    int v;
}

// The first loop merges a heap item with null: the merge register donated by
// the null side was not tracked as a GC slot, so the object arriving from the
// other side was never refcounted and each iteration leaked. The second loop
// merges two allocations: the addref for the merge was balanced, but the
// source register still owned a reference that was never dropped.
public void main() {
    for (int i = 0; i < 8; i++) {
        let a = (i % 2 == 0) ? new Box() : null;
        printnum(a == null ? 0 : 1);
    }
    for (int i = 0; i < 8; i++) {
        let b = (i % 2 == 0) ? new Box() : new Box();
        printnum(b == null ? 0 : 1);
    }
}
