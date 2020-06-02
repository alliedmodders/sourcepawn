#include <shell>

void dump_utf8(const char[] text, int len) {
    for (int i = 0; i < len; i++)
        printnum(text[i]);
}

public main() {
    char text[] = "{green} Cumpără Din Întâmplare";
    dump_utf8(text, sizeof(text));
}
