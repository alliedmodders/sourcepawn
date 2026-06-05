#include <shell>

int wow(int a, int b, int c, int d) {
    return a + b + c + d;
}

public void main() {
    int a = 0;
    for (int i = 0; i < 50000000; i++)
        a += wow(i, i + 2, i - 3, i + 5)
    printnum(a);
}
