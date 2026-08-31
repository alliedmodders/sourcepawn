#include <shell>

void f(intptr x = 100) {
    printnumptr(x);
}

void g(intptr x, intptr y = 200) {
    printnumptr(x);
    printnumptr(y);
}

public void main() {
    f();
    g(1);
    g(2, 3);
}
