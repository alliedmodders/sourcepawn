#include <shell>

typedef FuncType = function int (int x);

public void main() {
    let cb = function () -> FuncType { return function (int x) -> int { return x + 1; }; };
    printnum(cb()(5));
}
