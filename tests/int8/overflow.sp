#include <shell>

public main() {
    // int8 arithmetic is computed in the 32-bit cell, then narrowed to the
    // signed 8-bit range. Values that exceed the range wrap around.
    int8 a = 100;
    int8 b = 100;
    printnum(a + b);    // 200 -> -56
    printnum(a * 2);    // 200 -> -56
    printnum(a * 3);    // 300 -> 44
    printnum(-a);       // -100 (in range)
    printnum(~a);       // ~100 = -101 (in range)
    printnum(a >> 4);   // 6 (in range)
    printnum(a & 0xFF); // 100 (in range)

    // Assigning a wrapping expression to int8 stays wrapped.
    int8 c = a + b;    // -56
    printnum(c);

    // Negative overflow wraps too: -100 - 100 = -200 -> 56.
    int8 neg = -100;
    printnum(neg - a); // -200 -> 56

    // Comparisons produce a bool (not narrowed) and compare the in-range
    // int8 operands.
    printnum(a == b);   // 1
    printnum(c == a + b);// c and a+b are both -56 -> 1

    // In-range int8 constant arithmetic is allowed.
    const int8 A = 100;
    const int8 B = 27;
    printnum(A + B);    // 127

    // Mixed int8 + int promotes to int (no int8 narrowing).
    printnum(a + 5);    // 105
    int i = 5;
    printnum(a + i);    // 105

    // Storing an out-of-range value via an explicit cast round-trips.
    int8 packed = view_as<int8>(300);
    printnum(packed);   // 44
}
