#include <shell>

class C {
    int val;
    property int X {
        set(int v) { this.val = v; }
    }
}

public void main() {
    C c = new C();
    c.X = 5;
}
