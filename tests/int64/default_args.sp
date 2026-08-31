#include <shell>

void f(int64 x = 5) {
    printnum64(x);
}

void g(int64 x, int64 y = 7) {
    printnum64(x);
    printnum64(y);
}

public void main() {
    f();
    g(1);
    g(2, 3);
}
