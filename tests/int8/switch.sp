#include <shell>

public main() {
    int8 a = 5;
    switch (a) {
        case 5:
            printnum(1);
        default:
            printnum(2);
    }

    int8 b = -5;
    switch (b) {
        case -5:
            printnum(3);
        case 5:
            printnum(4);
        default:
            printnum(5);
    }

    int8 c = 127;
    switch (c) {
        case 127:
            printnum(6);
        default:
            printnum(7);
    }

    int8 d = -128;
    switch (d) {
        case -128:
            printnum(8);
        default:
            printnum(9);
    }

    int8 e = 0;
    switch (e) {
        case 0:
            printnum(10);
        default:
            printnum(11);
    }
}
