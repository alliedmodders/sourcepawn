#include <shell>

static int global[1] = { 7 };
int func(int param[1] = global) {
    return param[0]
}

public main() {
    printnum(func());
}
