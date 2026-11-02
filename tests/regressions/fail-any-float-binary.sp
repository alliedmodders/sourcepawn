// type: compiler-output
#include <shell>

stock any AnyFn(any x) {
    return x;
}

public void main() {
    any a = 2.5;

    printfloat(a * 2.0);
    printfloat(2.0 * a);
    printfloat(AnyFn(2.5) + 1.0);

    any neg = -2.5;
    printnum(neg < -3.0);

    int64 big = 1;
    any x = 1;
    printnum(x + big);
}
