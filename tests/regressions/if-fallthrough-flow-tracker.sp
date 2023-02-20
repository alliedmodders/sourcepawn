#include <shell>

int counter = 1;

int GetRandomInt() {
    return ++counter;
}

public void main() {
    int i = 3;
    do {
        printnum(i);
        i--;
        if (GetRandomInt() == 2)
            continue;
        break;
    } while (i);
}
