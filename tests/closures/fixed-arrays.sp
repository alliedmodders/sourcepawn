#include <shell>

typedef Callback = () -> int;

Callback GetFixedArrayCallback() {
    int arr[3] = {1, 2, 3};
    return function () -> int { return arr[0] + arr[1] + arr[2]; };
}

Callback GetSharedFixedArrayCallback() {
    shared int arr[3] = {1, 2, 3};
    let cb = function () -> int { return arr[0] + arr[1] + arr[2]; };
    arr[0] = 10; arr[1] = 20; arr[2] = 30;
    return cb;
}

public void main() {
    let cb1 = GetFixedArrayCallback();
    printnum(cb1());
    let cb2 = GetSharedFixedArrayCallback();
    printnum(cb2());
}
