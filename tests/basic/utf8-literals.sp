#include <shell>

public main() {
    char b2[] = "֎";
    char b3[] = "☃";
    char b4[] = "𒁗";

    printnum(sizeof(b2));
    printnum(b2[0]);
    printnum(b2[1]);

    print("---\n");

    printnum(sizeof(b3));
    printnum(b3[0]);
    printnum(b3[1]);
    printnum(b3[2]);

    print("---\n");

    printnum(sizeof(b4));
    printnum(b3[0]);
    printnum(b3[1]);
    printnum(b3[2]);
    printnum(b4[3]);
}
