#include <shell>

class C {
    int x;
    int y;
    C(int v) {
        this.x = v;
        this.y = v + 1;
    }
    property int P {
        get() { return this.y; }
    }
}

methodmap Clam {
    public Clam(int a) { return view_as<Clam>(a); }
    property int value {
        public get() { return 42; }
    }
}

public void main() {
    C c = new C(3);
    printnum(c.P);
    printnum(view_as<int>(c.P));
    printnum(view_as<int8>(c.P));

    Clam k = Clam(7);
    printnum(view_as<int>(k.value));
}
