// vim: set ts=2 sw=2 tw=99 et:
#include <shell>

void PokeRow(char t[6]) {
    t[1] = 'Z';
}

void ShowRow(const char t[6]) {
    print(t);
    print("\n");
}

public void main() {
    char buf[2][6];
    buf[0] = "hello";
    buf[1] = "hello";
    PokeRow(buf[1]);
    print(buf[1]);
    print("\n");
    ShowRow(buf[0]);
}
