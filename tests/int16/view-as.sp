#include <shell>

public main() {
    // view_as<int16> truncates to the low 16 bits and sign-extends.
    printnum(view_as<int16>(70000));    // 70000 & 0xffff = 4464, sign bit clear -> 4464
    printnum(view_as<int16>(-70000));   // low 16 bits 0xEE90, sign bit set -> -4464

    int64 w = 32767 + 1;                // 32768 -> 0x8000 -> -32768
    printnum(view_as<int16>(w));

    int i = 0x12345;
    printnum(view_as<int16>(i));        // low 16 bits 0x2345 -> 9029

    // Round-trip: an in-range value survives a view_as<int16>.
    printnum(view_as<int16>(-5));
}
