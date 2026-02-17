#include <shell>

#define test(%1) foo(%1)

void foo(const char[] p) {
    print(p);
}

public void main() {
        test("a,b");
}
