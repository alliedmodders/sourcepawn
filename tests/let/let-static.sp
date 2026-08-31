#include <shell>

void test() {
    static let x = 10;
    printnum(x);
    x += 5;
}

public void main() {
    test();
    test();
    test();
}
