// type: compiler-output

#include <shell>

public void main() {
    shared int v = 0;
    Function f = function () -> int { return v; };
    printnum(view_as<int>(f));
}
