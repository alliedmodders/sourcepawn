#include <shell>

enum struct Clams {
    int16 a;
    int16 b;
    int c;
}

class Widget {
    int16 id;
    int16 size;
}

public main() {
    Clams clam;
    clam.a = -100;
    clam.b = 200;
    clam.c = 3;
    printnum(clam.a);
    printnum(clam.b);
    printnum(clam.c);

    Widget w = new Widget();
    w.id = -7;
    w.size = 655;   // 655 == low 16 bits, sign clear
    printnum(w.id);
    printnum(w.size);
}
