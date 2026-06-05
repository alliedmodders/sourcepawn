#include <shell>

public main() {
    int a, b, c;
    for (int i = 0; i < 50000000; i++) {
        donothing_varargs(i, c, b, a);
    }
}

