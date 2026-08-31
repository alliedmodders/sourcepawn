#include <shell>

class C {
    int x;

    C(int v) { this.x = v; }

    static C create(int v) {
        return new C(v);
    }
}

public main()
{
    C c = C.create(42);
    printnum(c.x);
}
