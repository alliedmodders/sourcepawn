#include <shell>

public void main() {
    int x = 42;
    int result = (function () -> int { return x; })();
    printnum(result);
}
