#include <shell>

public main() {
    // int -> int16 requires an explicit cast for runtime values.
    int x = 100;
    int16 s = view_as<int16>(x);
    printnum(s);

    // int16 -> int is lossless sign-extension.
    int y = s;
    printnum(y);

    // Negative round-trips through int.
    int16 neg = -32768;
    int neg_int = neg;
    printnum(neg_int);

    // int16 -> float.
    float f = s;
    printfloat(f);

    // int16 -> int64.
    printnum64(s);
}
