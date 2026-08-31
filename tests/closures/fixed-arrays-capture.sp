#include <shell>

typedef Callback = () -> int;

Callback GetFixedArrayCallback() {
    int arr[3] = {1, 2, 3};
    return function () -> int { return arr[0] + arr[1] + arr[2]; };
}

public void main() {
    let cb1 = GetFixedArrayCallback();
    printnum(cb1());
}
