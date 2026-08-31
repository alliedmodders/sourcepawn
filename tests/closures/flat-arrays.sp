#include <shell>

typedef Callback = () -> int;

Callback GetFlatArrayCallback(int arr[3]) {
    return function () -> int {
        return arr[0] + arr[1] + arr[2];
    };
}

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
    int arr1[] = {1, 2, 3};
    let cb1 = GetFlatArrayCallback(arr1);
    arr1[0] = 100; 
    printnum(cb1()); // Fixed-size array args are deep-copied, so cb1 sees 1+2+3

    int arr2[] = {1, 2, 3};
    let cb2 = GetSharedFlatArrayCallback(arr2);
    printnum(cb2());
}
