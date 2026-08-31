#include <shell>

typedef Callback = () -> int;

Callback GetSharedFlatArrayCallback(int in_arr[3]) {
    shared int arr[3];
    arr = in_arr;
    let cb = function () -> int {
        return arr[0] + arr[1] + arr[2];
    };
    arr[0] = 10; arr[1] = 20; arr[2] = 30;
    return cb;
}

public void main() {
    int arr2[] = {1, 2, 3};
    let cb2 = GetSharedFlatArrayCallback(arr2);
    printnum(cb2()); // expects 60: reassignment is deep copy, mutations visible via class field
}
