#include <shell>

public main() {
    // int -> int8 requires an explicit cast for runtime values.
    int x = 100;
    int8 s = view_as<int8>(x);
    printnum(s);

    // int8 -> int is lossless sign-extension.
    int y = s;
    printnum(y);

    // Negative round-trips through int.
    int8 neg = -128;
    int neg_int = neg;
    printnum(neg_int);

    // int8 -> int16 sign-extends (lossless).
    int16 w16 = neg;
    printnum(w16);

    // In-range constant int16 -> int8 narrows implicitly.
    const int16 C16 = 100;
    int8 back = C16;
    printnum(back);

    // int8 -> float.
    float f = s;
    printfloat(f);

    // int8 -> int64.
    printnum64(s);
}
