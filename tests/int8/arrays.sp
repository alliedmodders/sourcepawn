#include <shell>

int8 g_blah[5] = { 1, -2, 3, -4, 127 };

void dump(int8[] arr, int n) {
    for (int i = 0; i < n; i++)
        printnum(arr[i]);
}

public main() {
    // Static local array with initializer.
    int8 a[5] = { 10, -20, 30, -40, 127 };
    printnum(sizeof(a));
    printnum(a[0]);
    printnum(a[1]);
    printnum(a[2]);
    printnum(a[3]);
    printnum(a[4]);

    // Indexed write.
    a[1] = -128;
    printnum(a[1]);

    // Global array.
    printnum(g_blah[0]);
    printnum(g_blah[4]);

    // Heap array.
    int8[] h = new int8[3];
    h[0] = 5;
    h[1] = -6;
    h[2] = 7;
    printnum(h[0]);
    printnum(h[1]);
    printnum(h[2]);

    // Slicing / passing to a function.
    dump(a, 3);
}
