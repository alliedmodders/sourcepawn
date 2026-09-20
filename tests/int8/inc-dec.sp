#include <shell>

int8 g_counter = 126;

enum struct Clams {
    int8 a;
}

class Widget {
    int8 id;
}

public main() {
    // Local wraparound at both boundaries.
    int8 a = 127;
    a++;
    printnum(a);        // -128
    a++;
    printnum(a);        // -127
    a = -128;
    a--;
    printnum(a);        // 127

    // Prefix and postfix expression values must be narrowed too.
    int8 b = 126;
    printnum(++b);      // 127
    printnum(++b);      // -128
    printnum(b++);      // -128
    printnum(b);        // -127

    // Globals.
    g_counter++;
    printnum(g_counter);    // 127
    g_counter++;
    printnum(g_counter);    // -128

    // Class fields.
    Widget w = new Widget();
    w.id = 127;
    w.id++;
    printnum(w.id);         // -128
    w.id--;
    printnum(w.id);         // 127

    // Enum struct fields.
    Clams clam;
    clam.a = 127;
    clam.a++;
    printnum(clam.a);       // -128

    // Upvars captured by a closure.
    int8 n = 127;
    let cb = function () -> int {
        n++;
        return n;
    };
    printnum(cb());         // -128

    // Array elements narrow via stor.elem.i8 and must stay wrapped.
    int8 arr[2];
    arr[0] = 127;
    arr[1] = -128;
    arr[0]++;
    arr[1]--;
    printnum(arr[0]);       // -128
    printnum(arr[1]);       // 127

    // In-range inc/dec is unchanged.
    int8 c = 5;
    c++;
    c--;
    printnum(c);            // 5
}
