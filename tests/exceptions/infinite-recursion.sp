// returnCode: 1
#include <shell>

int recurse(int n) {
    return recurse(n + 1);
}

public void main() {
    recurse(0);
}
