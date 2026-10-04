#include <shell>

void Show(char buf[8]) {
    print(buf);
    print("\n");
}

void Pair(char a[4], char b[8] = "def") {
    print(a);
    print(",");
    print(b);
    print("\n");
}

public void main() {
    Show("hello");
    Show("");
    Pair("abc");
    Pair("xyz", "1234567");
}
