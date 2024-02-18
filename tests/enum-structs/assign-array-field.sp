#include <shell>

enum struct Blah {
    char str[32];
}

public main() {
    Blah blah
    blah.str = "hello\n"
    print(blah.str);
}
