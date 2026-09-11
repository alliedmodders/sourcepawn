// type: compiler-output
#include <shell>

enum struct X {
    int a[700000000];
    int b[1073741824];
}

public void main() {
    X x;
    printnum(sizeof(x));
}
