#include <shell>

public main() {
    float f = 5.0;
    switch (f) {
        case 5.0:
            printnum(1);
        default:
            printnum(2);
    }

    float g = 0.0;
    switch (g) {
        case 2.5:
            printnum(3);
        default:
            printnum(4);
    }
}
