// vim: set ts=2 sw=2 tw=99 et:
#include <shell>

enum Tag { Tag_A = 1 };

void ShowTag(Tag:x[]) {
    int c = view_as<int>(x[0]);
    printnum(c & 0xff);
    print("\n");
}

void ShowTagFixed(Tag x[6]) {
    int c = view_as<int>(x[1]);
    printnum(c & 0xff);
    print("\n");
}

public void main() {
    ShowTag("hello");
    ShowTagFixed("hello");
}
