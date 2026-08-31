#include <shell>

enum struct ES {
    int x;
    int y;
}

class C {
    ES es;
    C() {
        ES local;
        this.es.x = 10;
        this.es.y = 20;
    }
}

public main()
{
    C c = new C();
    printnum(c.es.x);
    printnum(c.es.y);
    return 0;
}
