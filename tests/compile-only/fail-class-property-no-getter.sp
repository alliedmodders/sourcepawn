#include <shell>

class C {
    property int X {
        set(int v) {}
    }
}

public void main() {
    C c = new C();
    printnum(c.X);
}
