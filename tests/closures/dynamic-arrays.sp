#include <shell>

typedef Callback = () -> int;

Callback GetDynamicArrayCallback(int size) {
    int[] arr = new int[size];
    for (int i = 0; i < size; i++) arr[i] = i + 1;
    return function () -> int { return arr[0] + arr[1] + arr[2]; };
}

Callback GetSharedDynamicArrayCallback(int size) {
    shared int[] arr = new int[size];
    for (int i = 0; i < size; i++) arr[i] = i + 1;
    let cb = function () -> int { return arr[0] + arr[1] + arr[2]; };
    for (int i = 0; i < size; i++) arr[i] *= 10;
    return cb;
}

public void main() {
    printnum(GetDynamicArrayCallback(3)());
    printnum(GetSharedDynamicArrayCallback(3)());
}
