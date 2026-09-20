#include <shell>

float g[3] = {1, 2, 3};
float g2[3] = {1, 2.5, 3};

public main() {
    float a[3] = {1, 2, 3};
    printfloat(a[0]);
    printfloat(a[1]);
    printfloat(a[2]);

    float b[3] = {1, 2.5, 3};
    printfloat(b[0]);
    printfloat(b[1]);
    printfloat(b[2]);

    printfloat(g[0]);
    printfloat(g[1]);
    printfloat(g[2]);

    printfloat(g2[0]);
    printfloat(g2[1]);
    printfloat(g2[2]);

    // Exceeds the flat array cutoff, forcing the fixed-array fill path.
    float big[1025] = {1, 2, 3};
    printfloat(big[0]);
    printfloat(big[1]);
    printfloat(big[2]);
    printfloat(big[1024]);
}
