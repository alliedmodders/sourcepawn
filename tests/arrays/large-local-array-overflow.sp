#include <shell>

// Regression: a flat array of more than 4096 bytes would overflow the
// uint16_t vreg space during v2 lowering (assertion in
// lowering.cpp:191 emitVal). The compiler now downgrades such arrays
// to heap-allocated FixedArray.
public main() {
    int arr[65534];
    arr[0] = 42;
    arr[42] = 99;
    arr[65533] = 1234;
    printnum(arr[0]);
    printnum(arr[42]);
    printnum(arr[65533]);
    return 0;
}
