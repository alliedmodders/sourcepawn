#include <shell>

typedef Callback = () -> int;

Callback GetCallback() {
    int[] arr = new int[5];
    arr[0] = 42;
    // Non-shared capture - reassigning to null must release the old heap item.
    Callback cb = function () -> int {
        arr = null;
        return 0;
    };
    return cb;
}

public void main() {
    Callback cb = GetCallback();
    printnum(cb());
    print("done\n");
}
