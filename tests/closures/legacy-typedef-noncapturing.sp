#include <shell>

typedef Cb = function void ();

public void main() {
    Cb f = function () -> void { printnum(5); };
    printnum(execute(f, 1));
}
