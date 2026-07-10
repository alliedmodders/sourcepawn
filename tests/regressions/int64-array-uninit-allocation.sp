#include <shell>

int64 arr[8];
int64 guard[8] = {0, 0, 0, 0, 0, 0, 0, 0};

public void main() {
    for (int i = 0; i < 8; i++)
        arr[i] = 100 + i;
    for (int i = 0; i < 8; i++)
        printnum64(guard[i]);
}
