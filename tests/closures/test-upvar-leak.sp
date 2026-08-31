#include <shell>

public void main() {
    shared int x = 0;
    let g = function () {
        x = 5;
    };
    g();
    printnum(x);
}
