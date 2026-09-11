#include <shell>

int g_counter;

int bump() {
    g_counter++;
    return 0;
}

void main() {
    let a = new int[100];
    int b[4];

    printnum(a.size);
    printnum(b.size);

    // Statically-sized arrays must still evaluate side effects in the base.
    int c[2];
    int d[2];
    d[0] = 11;
    d[1] = 22;
    printnum((c = d).size);
    printnum(c[0] + c[1]);

    int m[2][4];
    printnum(m[bump()].size);
    printnum(g_counter);
}
