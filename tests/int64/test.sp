#include <shell>

public main() {
    int64 a = 10;
    int64 b = 0;
    if (a)
        print("10 is true\n");
    if (!a)
        print("should not get here\n");
    if (b)
        print("should not get here either\n");
    if (!b)
        print("0 is false\n");
}
