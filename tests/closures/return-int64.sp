#include <shell>

public void main() {
    let cb = function () -> int64 { return 42; };
    printnum64(cb());
}
