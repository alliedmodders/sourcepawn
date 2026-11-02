#include <shell>

public void main() {
    any a = 3;
    a += 1;
    printnum(a);
    a *= 2;
    printnum(a);
    a -= 4;
    printnum(a);
    a /= 2;
    printnum(a);
    a %= 4;
    printnum(a);
    a <<= 3;
    printnum(a);
    a >>= 2;
    printnum(a);
    a &= 0xff;
    printnum(a);
    a |= 0x100;
    printnum(a);
    a ^= 1;
    printnum(a);
}
