// Exercises intptr array fill (EmitArrayFillIntptr path), which the compiler
// emits per-element instead of using OP_FILLARRAY. The wire format is 4
// bytes per intptr; the VM sign-extends each element into an intptr_t slot.
#include <shell>

intptr g_arr[4] = { -2147483648, 0, 2147483647, -1 };

void dump(intptr[] arr, int n) {
    for (int i = 0; i < n; i++)
        printnumptr(arr[i]);
}

public main() {
    // Static local intptr array with initializer.
    intptr a[4] = { -2147483648, 0, 2147483647, -1 };
    printnumptr(a[0]);
    printnumptr(a[1]);
    printnumptr(a[2]);
    printnumptr(a[3]);

    // Heap-allocated intptr array, assigned per-element.
    intptr[] h = new intptr[4];
    h[0] = -2147483648;
    h[1] = 0;
    h[2] = 2147483647;
    h[3] = -1;
    printnumptr(h[0]);
    printnumptr(h[1]);
    printnumptr(h[2]);
    printnumptr(h[3]);

    // Global intptr array.
    printnumptr(g_arr[0]);
    printnumptr(g_arr[1]);
    printnumptr(g_arr[2]);
    printnumptr(g_arr[3]);

    // Pass array through function (slicing).
    dump(a, 4);
}
