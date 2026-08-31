#include <shell>

typedef FuncType = (int x) -> int;

public void main() {
    let cb = function () -> FuncType { return function (int x) -> int { return x + 1; }; };
    printnum(cb()(5));
}
