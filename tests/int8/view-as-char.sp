#include <shell>

public main() {
    // view_as<char>(int) does NOT truncate to the low 8 bits.
    // This is the legacy char behavior: truncation only happens on array loads.
    // The compiler emits no CVT opcode for char targets, so the full source
    // cell value flows unmodified into the char slot. For negative values
    // the char slot ends up sign-extended (high 3 bytes = 0xFF). This is
    // intentional, not a bug.

    // Positive int with non-zero high bits: char slot gets the full int.
    int x = 0x12345678;
    char c = view_as<char>(x);
    printnum(c);    // 305419896 (NOT 120 / 0x78)

    // Negative int: char slot gets the sign-extended int.
    int y = -1;
    char d = view_as<char>(y);
    printnum(d);    // -1 (NOT 255)

    // Boundary: int = 256 -- no truncation happens.
    int z = 256;
    char e = view_as<char>(z);
    printnum(e);    // 256 (NOT 0)

    // Same applies to int16/int64 sources (all stay at full cell width).
    int16 q = 0x1234;
    char f = view_as<char>(q);
    printnum(f);    // 4660 (NOT 0x34 = 52)
}
