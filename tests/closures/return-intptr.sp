#include <shell>

public void main() {
    let cb = function () -> intptr { return 42; };
    printnum(cb());
}
