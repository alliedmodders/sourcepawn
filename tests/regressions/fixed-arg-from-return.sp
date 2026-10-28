// vim: set ts=2 sw=2 tw=99 et:
#include <shell>

void ShowFixed(const char t[6]) {
    print(t);
    print("\n");
}

void PokeFixed(char t[6]) {
    t[1] = 'Z';
}

char[] GetHello() {
    return "hello";
}

public void main() {
    ShowFixed(GetHello());
    PokeFixed(GetHello());
    print("done");
    print("\n");
}
