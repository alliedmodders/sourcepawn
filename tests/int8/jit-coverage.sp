#include <shell>

// LL_LOAD_I_I8 + LL_STOR_I_I8: int8 reference parameter. The indirect store
// goes through HeapAddr, whose base register (r15) needs a REX byte.
void inc(int8& x) {
    printnum(x);      // load.i.i8
    x++;              // load.i.i8 + stor.i.i8
    printnum(x);      // load.i.i8 after store
}

public main() {
    int8 v = -100;
    inc(v);
    printnum(v);                              // -99
}
