// vim: set ts=2 sw=2 tw=99 et:
#include <shell>

void ShowFixed(const char t[6]) {
    print(t);
    print("\n");
}

void PokeFixed(char t[6]) {
    t[1] = 'Z';
}

void PokeDefaultFixed(char t[6] = "hello") {
    t[2] = 'Y';
}

public void main() {
    ShowFixed("hello");
    PokeFixed("hello");
    print("hello");
    print("\n");
    PokeFixed(true ? "hello" : "hello");
    print("hello");
    print("\n");
    PokeDefaultFixed();
    print("hello");
    print("\n");
}
