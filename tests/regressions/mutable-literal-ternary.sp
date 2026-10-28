// vim: set ts=2 sw=2 tw=99 et:
#include <shell>

void Poke(char[] s) {
    s[1] = 'Z';
}

void PokeFixed(char x[6]) {
    x[1] = 'Y';
}

void Show(char[] s) {
    print(s);
    print("\n");
}

char[] Take(int c) {
    return c ? "hello" : "world";
}

stock int Cond() {
    int x = 1;
    return x;
}

public void main() {
    Poke(Cond() ? "hello" : "world");
    Show(Cond() ? "hello" : "world");
    Poke(Cond() ? "hi" : "world");
    Show(Cond() ? "hi" : "world");
    PokeFixed(Cond() ? "hello" : "hello");
    Show(Cond() ? "hello" : "hello");
    Poke(Cond() ? (Cond() ? "hello" : "hello") : "world");
    Show(Cond() ? (Cond() ? "hello" : "hello") : "world");
    Poke(Take(Cond()));
    Show(Take(Cond()));
}
