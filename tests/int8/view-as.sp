#include <shell>

public main() {
    // view_as<int8> truncates to the low 8 bits and sign-extends.
    printnum(view_as<int8>(300));      // 300 & 0xff = 44, sign bit clear -> 44
    printnum(view_as<int8>(-300));     // low 8 bits 0xD4, sign bit set -> -44

    int64 w = 127 + 1;                // 128 -> 0x80 -> -128
    printnum(view_as<int8>(w));

    int i = 0x12345;
    printnum(view_as<int8>(i));       // low 8 bits 0x45 -> 69

    // int16 source: truncates to the low 8 bits and sign-extends.
    int16 q = -300;
    printnum(view_as<int8>(q));       // low 8 bits 0xD4, sign bit set -> -44

    // char and int8 are both 1-byte integers; view_as between them is legal.
    char ch = 'A';
    printnum(view_as<int8>(ch));      // 65
    int8 v8 = 65;
    printnum(view_as<char>(v8));      // 65

    // Round-trip: an in-range value survives a view_as<int8>.
    printnum(view_as<int8>(-5));
}
