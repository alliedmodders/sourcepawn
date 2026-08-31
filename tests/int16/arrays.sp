#include <shell>

int16 g_blah[5] = { 1, -2, 3, -4, 32767 };

void dump(int16[] arr, int n) {
    for (int i = 0; i < n; i++)
        printnum(arr[i]);
}

public main() {
    // Static local array with initializer.
    int16 a[5] = { 10, -20, 30, -40, 32767 };
    printnum(sizeof(a));
    printnum(a[0]);
    printnum(a[1]);
    printnum(a[2]);
    printnum(a[3]);
    printnum(a[4]);

    // Indexed write.
    a[1] = -32768;
    printnum(a[1]);

    // Global array.
    printnum(g_blah[0]);
    printnum(g_blah[4]);

    // Heap array.
    int16[] h = new int16[3];
    h[0] = 5;
    h[1] = -6;
    h[2] = 7;
    printnum(h[0]);
    printnum(h[1]);
    printnum(h[2]);

    // Slicing / passing to a function.
    dump(a, 3);
}
