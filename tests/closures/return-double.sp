#include <shell>

native void printdouble(double n);

public void main() {
    let cb = function () -> double { return 3.14; };
    printdouble(cb());
}
