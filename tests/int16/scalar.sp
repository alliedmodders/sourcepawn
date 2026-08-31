#include <shell>

public main() {
    int16 a = 10;
    int16 b = a;
    printnum(a);
    printnum(b);

    int16 c = -5;
    printnum(c);

    // Arithmetic on int16 operands (computed in the cell, then stored).
    int16 sum = a + b;         // 10 + 10 = 20
    int16 diff = c - a;        // -5 - 10 = -15
    printnum(sum);
    printnum(diff);

    // Comparisons.
    printnum(a == b);          // 1
    printnum(a < c);           // 0
    printnum(c < a);           // 1
    printnum(a != sum);        // 1
    printnum(sum > diff);      // 1
}
