// type: compiler-output

#include <shell>

typeset TS {
    function int ();
}

public void main() {
    shared int v = 0;
    TS f = function () -> int { return v; };
    printnum(view_as<int>(f));
}
