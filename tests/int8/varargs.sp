#include <shell>

public main() {
    int8 a[8];
    a[0] = 1;
    a[1] = 2;
    a[2] = -3;
    a[3] = 100;
    printnums(a[1]);
    printnums(a[0], a[1], a[2], a[3]);
    int c[4];
    c[1] = 77;
    printnums(c[1]);
}
