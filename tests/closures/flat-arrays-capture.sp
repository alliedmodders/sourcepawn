#include <shell>

typedef Callback = () -> int;

Callback GetFlatArrayCallback(int arr[3]) {
    return function () -> int {
        return arr[0] + arr[1] + arr[2];
    };
}

public void main() {
    int arr1[] = {1, 2, 3};
    let cb1 = GetFlatArrayCallback(arr1);
    arr1[0] = 100;
    printnum(cb1()); // Fixed-size array args are deep-copied, so cb1 sees 1+2+3
}
