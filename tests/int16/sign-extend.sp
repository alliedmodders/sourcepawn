#include <shell>

public main() {
    // Extremes round-trip exactly (signed, sign-extending).
    int16 lo = -32768;
    int16 hi = 32767;
    printnum(lo);
    printnum(hi);

    // Reading back into an int must sign-extend.
    int lo_int = lo;
    int hi_int = hi;
    printnum(lo_int);
    printnum(hi_int);

    // Negative values via arithmetic and constants.
    int16 neg = -1;
    printnum(neg);
    // int16 minus an int literal promotes to int; narrowing back to int16 is
    // explicit (view_as).
    int16 from_expr = view_as<int16>(hi - 1);
    printnum(from_expr);
}
