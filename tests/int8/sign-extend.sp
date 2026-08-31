#include <shell>

public main() {
    // Extremes round-trip exactly (signed, sign-extending).
    int8 lo = -128;
    int8 hi = 127;
    printnum(lo);
    printnum(hi);

    // Reading back into an int must sign-extend.
    int lo_int = lo;
    int hi_int = hi;
    printnum(lo_int);
    printnum(hi_int);

    // Negative values via arithmetic and constants.
    int8 neg = -1;
    printnum(neg);
    // int8 minus an int literal promotes to int; narrowing back to int8 is
    // explicit (view_as).
    int8 from_expr = view_as<int8>(hi - 1);
    printnum(from_expr);
}
