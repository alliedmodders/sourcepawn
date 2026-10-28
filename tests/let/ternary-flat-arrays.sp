#include <shell>

void ShowMixed(bool c) {
    char a[6] = "hello";
    print(c ? a : "world");
}

void ShowBothFlat(bool c) {
    char a[6] = "hello";
    char b[6] = "world";
    print(c ? a : b);
}

char[] Ret(bool c) {
    char a[6] = "hello";
    return c ? a : "world";
}

public void main() {
    ShowBothFlat(false);
    print("\n");
    ShowBothFlat(true);
    print("\n");
    ShowMixed(false);
    print("\n");
    ShowMixed(true);
    print("\n");
    print(Ret(false));
    print("\n");
    print(Ret(true));
    print("\n");
    printstrs(true ? "yes" : "no", "\n");
    printstrs(false ? "yes" : "no", "\n");
    char a[6] = "hello";
    char b[6] = "world";
    print(true ? a : b);
    print("\n");
    print(false ? a : b);
    print("\n");
}
