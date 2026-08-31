#include <shell>

// Minimal reproduction: JIT does not release local heap items on function return.
// The local array `arr` is leaked in JIT mode but correctly freed in interpreter mode.

int MakeAndLeak() {
    int[] arr = new int[10];
    arr[0] = 42;
    return arr[0];
}

public void main() {
    printnum(MakeAndLeak());
    print("done\n");
}
