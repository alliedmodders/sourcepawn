#include <shell>

public main() {
    float a1 = 1.0;
    for (int i = 0; i < 100000000; i++) {
        a1 += 1.0;
        a1 -= 1.0;
        a1 *= 1.0;
        a1 /= 1.0;

        a1 += 1.0;
        a1 -= 1.0;
        a1 *= 1.0;
        a1 /= 1.0;
    }
    printfloat(a1);
}
