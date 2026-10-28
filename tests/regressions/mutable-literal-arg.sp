// vim: set ts=2 sw=2 tw=99 et:
#include <shell>

void Poke(char[] s) {
    s[1] = 'Z';
}

char[] GetHello() {
    return "hello";
}

void PokeDefault(char[] s = "hello") {
    s[1] = 'X';
}

public void main() {
    Poke("hello");
    print("hello");
    print("\n");
    Poke(GetHello());
    print("hello");
    print("\n");
    PokeDefault();
    print("hello");
    print("\n");
    printstrs("hello");
    print("\n");
    Poke(true ? "hello" : "goodbye");
    print("hello");
    print("\n");
}
