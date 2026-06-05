#include <shell>

public void main() {
    int x = 0;
    for (int i = 0; i < 50000000; i++)
        x += i;
    printnum(x);
}
