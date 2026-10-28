#include <shell>

void Foo(const char s[5000] = "hello") {
    print(s);
    print("\n");
}

public void main() {
    Foo("hi");
    Foo();
}
