#include <shell>

public main()
{
    // When the true branch of a ternary is a 32-bit constant and the false
    // branch is a wide type, the coercion is applied to the true branch.
    // The ternary result type used to stay int, which caused a redundant
    // cvt.i64 to be emitted over an int64 value, and the VM rejected the
    // binary (or failed at runtime with verification disabled).
    int64 a = 5;
    int c = 1;
    printnum64(c > 0 ? 31 : a + 1);
    printnum64(c > 1 ? 31 : a + 1);

    intptr x = 5;
    printnumptr(c > 0 ? 3 : x + 1);
    printnumptr(c > 1 ? 3 : x + 1);

    int64 v = c > 0 ? 11 : a + 1;
    printnum64(v);
}
