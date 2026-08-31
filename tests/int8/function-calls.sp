#include <shell>

int8 make_negative() {
    return -42;
}

int8 negate(int8 v) {
    return -v;
}

int8 pass_through(int8 v) {
    return v;
}

void report(int8 v) {
    printnum(v);
}

public main() {
    printnum(make_negative());
    printnum(negate(7));
    printnum(pass_through(-128));
    report(make_negative());
}
