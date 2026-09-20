#include <shell>

int16 g_counter = 32766;

enum struct Clams {
    int16 a;
}

class Widget {
    int16 id;
}

public main() {
    // Local wraparound at both boundaries.
    int16 a = 32767;
    a++;
    printnum(a);        // -32768
    a++;
    printnum(a);        // -32767
    a = -32768;
    a--;
    printnum(a);        // 32767

    // Prefix and postfix expression values must be narrowed too.
    int16 b = 32766;
    printnum(++b);      // 32767
    printnum(++b);      // -32768
    printnum(b++);      // -32768
    printnum(b);        // -32767

    // Globals.
    g_counter++;
    printnum(g_counter);    // 32767
    g_counter++;
    printnum(g_counter);    // -32768

    // Class fields.
    Widget w = new Widget();
    w.id = 32767;
    w.id++;
    printnum(w.id);         // -32768
    w.id--;
    printnum(w.id);         // 32767

    // Enum struct fields.
    Clams clam;
    clam.a = 32767;
    clam.a++;
    printnum(clam.a);       // -32768

    // Upvars captured by a closure.
    int16 n = 32767;
    let cb = function () -> int {
        n++;
        return n;
    };
    printnum(cb());         // -32768

    // Array elements narrow via stor.elem.i16 and must stay wrapped.
    int16 arr[2];
    arr[0] = 32767;
    arr[1] = -32768;
    arr[0]++;
    arr[1]--;
    printnum(arr[0]);       // -32768
    printnum(arr[1]);       // 32767

    // In-range inc/dec is unchanged.
    int16 c = 5;
    c++;
    c--;
    printnum(c);            // 5
}
