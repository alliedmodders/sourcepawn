#include <shell>

public main() {
    // int16 arithmetic is computed in the 32-bit cell, then narrowed to the
    // signed 16-bit range. Values that exceed the range wrap around.
    int16 a = 30000;
    int16 b = 30000;
    printnum(a + b);    // 60000 -> -5536
    printnum(a * 2);    // 60000 -> -5536
    printnum(a * 3);    // 90000 -> 24464
    printnum(-a);       // -30000 (in range)
    printnum(~a);       // ~30000 = -30001 (in range)
    printnum(a >> 4);   // 1875 (in range)
    printnum(a & 0xFFFF);// 30000 (in range)

    // Assigning a wrapping expression to int16 stays wrapped.
    int16 c = a + b;    // -5536
    printnum(c);

    // Negative overflow wraps too: -30000 - 30000 = -60000 -> 5536.
    int16 neg = -30000;
    printnum(neg - a);  // -60000 -> 5536

    // Comparisons produce a bool (not narrowed) and compare the in-range
    // int16 operands.
    printnum(a == b);   // 1
    printnum(c == a + b);// c and a+b are both -5536 -> 1

    // In-range int16 constant arithmetic is allowed.
    const int16 A = 30000;
    const int16 B = 1000;
    printnum(A + B);    // 31000

    // Mixed int16 + int promotes to int (no int16 narrowing).
    printnum(a + 5);    // 30005
    int i = 5;
    printnum(a + i);    // 30005

    // Storing an out-of-range value via an explicit cast round-trips.
    int16 packed = view_as<int16>(60000);
    printnum(packed);   // -5536
}
