#include <shell>

enum struct Clams {
    int8 a;
    int8 b;
    int c;
}

class Widget {
    int8 id;
    int8 size;
}

public main() {
    Clams clam;
    clam.a = -100;
    clam.b = 100;
    clam.c = 3;
    printnum(clam.a);
    printnum(clam.b);
    printnum(clam.c);

    Widget w = new Widget();
    w.id = -7;
    w.size = 100;
    printnum(w.id);
    printnum(w.size);
}
