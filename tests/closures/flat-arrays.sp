#include <shell>

typedef Callback = () -> int;

Callback GetFlatArrayCallback(int[] arr) {
    return function () -> int {
        return arr[0] + arr[1] + arr[2];
    };
}

Callback GetSharedFlatArrayCallback(int[] in_arr) {
    int[] arr = in_arr;
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
    printnum(cb1()); // Flat arrays are passed by reference, mutating original affects the closure

    int arr2[] = {1, 2, 3};
    let cb2 = GetSharedFlatArrayCallback(arr2);
    printnum(cb2());
}
