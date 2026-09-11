#include <shell>

enum struct S {
    int16 w;
    int16 arr[3];
}

public main() {
    int16 a[4];
    a[0] = -300;
    a[1] = 2;
    a[2] = -1000;
    a[3] = 12345;
    printnums(a[0]);
    printnums(a[0], a[1], a[2], a[3]);

    S s;
    s.arr[0] = -300;
    s.arr[1] = 500;
    printnums(s.arr[0]);
    printnums(s.arr[1]);
}
