#include <shell>

native void printdouble(double n);

void f(double x = 1.5d) {
    printdouble(x);
}

void g(double x, double y = 2.5d) {
    printdouble(x);
    printdouble(y);
}

public void main() {
    f();
    g(0.5d);
    g(1.5d, 2.0d);
}
