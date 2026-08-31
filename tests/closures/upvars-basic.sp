#include <shell>

typedef Callback = () -> int;

Callback GetCallback1(int n) {
    return function () -> int { return ++n; };
}

Callback GetCallback2(int in_n) {
    shared int n = in_n;
    let other = function () -> int { return ++n; };
    n *= 10;
    return other;
}

public void main() {
    let c1 = GetCallback1(10);
    printnum(c1());
    printnum(c1());
    printnum(c1());
    let c2 = GetCallback2(20);
    printnum(c2());
    printnum(c2());
    printnum(c2());
}
