#include <shell>

int16 g_data[4] = { 100, -200, 300, -32768 };

int16 read_through(int16[] arr, int i) {
    return arr[i];
}

void bump(int16& x) {
    printnum(x);      // load.i.i16
    x++;              // load.i.i16 + stor.i.i16
    printnum(x);      // load.i.i16 after store
}

public main() {
    // LL_CVT_I16: view_as<int16> truncates + sign-extends.
    printnum(view_as<int16>(0x12345));        // 0x2345 = 9029
    printnum(view_as<int16>(-1));             // -1
    printnum(view_as<int16>(0x10000));        // 0 (truncated away)

    // LL_LOAD_ELEM_FLAT_I16 + LL_STOR_ELEM_FLAT_I16: static local int16 array.
    // The store-then-read pattern guards against the movw corruption regression:
    // a 16-bit store must not clobber adjacent elements.
    int16 local[4] = { 11, -22, 33, -44 };
    local[1] = 200;
    local[2] = -300;
    printnum(local[0]);                       // 11
    printnum(local[1]);                       // 200
    printnum(local[2]);                       // -300
    printnum(local[3]);                       // -44 (must survive the stores)

    // LL_LOAD_ELEM_FLAT_I16 (global).
    printnum(g_data[3]);                      // -32768

    // LL_STOR_ELEM_I16 + LL_LOAD_ELEM_I16: heap int16 array.
    int16[] heap = new int16[2];
    heap[0] = -12345;
    heap[1] = 12345;
    printnum(heap[0]);
    printnum(heap[1]);

    // LL_LOAD_ELEM_FLAT_I_I16: indexed access on an int16[] argument.
    printnum(read_through(g_data, 1));        // -200
    printnum(read_through(local, 2));         // -300

    // LL_LOAD_I_I16 + LL_STOR_I_I16: int16 reference parameter. The indirect
    // store goes through HeapAddr, whose base register (r15) needs a REX
    // byte; guards against the movw prefix-order bug that dropped the REX
    // and sent the 16-bit store to a wild address.
    int16 v = -200;
    bump(v);
    printnum(v);                              // -199
}
