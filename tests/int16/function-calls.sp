#include <shell>

int16 make_negative() {
    return -42;
}

int16 negate(int16 v) {
    return -v;
}

int16 pass_through(int16 v) {
    return v;
}

void report(int16 v) {
    printnum(v);
}

public main() {
    printnum(make_negative());
    printnum(negate(7));
    printnum(pass_through(-32768));
    report(make_negative());
}
