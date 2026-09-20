#include <shell>

public main() {
    int16 a = 1000;
    switch (a) {
        case 1000:
            printnum(1);
        default:
            printnum(2);
    }

    int16 b = -32768;
    switch (b) {
        case -32768:
            printnum(3);
        default:
            printnum(4);
    }

    int16 c = 32767;
    switch (c) {
        case 32767:
            printnum(5);
        default:
            printnum(6);
    }

    int16 d = 5;
    switch (d) {
        case 5:
            printnum(7);
        default:
            printnum(8);
    }
}
