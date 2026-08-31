#include <shell>

class C {
    int x;
    C(int v) { this.x = v; }
}

void setVal(int& ref, int val) {
    ref = val;
}

public main()
{
    C c = new C(42);
    setVal(c.x, 99);
    printnum(c.x);
    return 0;
}
