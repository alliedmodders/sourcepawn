#include <shell>

public main() {
    int64 x = 5;
    int64 z = 0;
    int64 n = -1;

    printnum((x) ? 1 : 0);
    printnum((z) ? 1 : 0);
    printnum((n) ? 1 : 0);

    int y = (x) ? 1 : 0;
    printnum(y);

    printnum((x > 3) ? 10 : 20);
    printnum((x < 3) ? 10 : 20);

    int64 result = (x) ? 100 : 200;
    printnum64(result);

    int64 r2 = (z) ? 100 : 200;
    printnum64(r2);

    printnum(((x) ? 1 : 0) ? 100 : 200);
    printnum(((z) ? 1 : 0) ? 100 : 200);
}
