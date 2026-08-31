#include <shell>

public main() {
    int8 a = 10;
    int8 b = a;
    printnum(a);
    printnum(b);

    int8 c = -5;
    printnum(c);

    // Arithmetic on int8 operands (computed in the cell, then narrowed).
    int8 sum = a + b;         // 10 + 10 = 20
    int8 diff = c - a;        // -5 - 10 = -15
    printnum(sum);
    printnum(diff);

    // Comparisons.
    printnum(a == b);          // 1
    printnum(a < c);           // 0
    printnum(c < a);           // 1
    printnum(a != sum);        // 1
    printnum(sum > diff);      // 1
}
