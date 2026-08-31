#include <shell>

int[] gArr = null;

public void main() {
    gArr = new int[5];
    gArr[0] = 42;
    printnum(gArr[0]);
    gArr = null;
    print("done\n");
}
