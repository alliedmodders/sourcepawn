// type: compiler-output

#include <shell>

public void main() {
    shared int v = 0;
    any x = function () -> int { return v; };
    printnum(view_as<int>(x) != 0);
}
