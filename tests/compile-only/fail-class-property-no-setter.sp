#include <shell>

class C {
    int val;
    property int X {
        get() { return this.val; }
    }
}

public void main() {
    C c = new C();
    c.X = 5;
}
