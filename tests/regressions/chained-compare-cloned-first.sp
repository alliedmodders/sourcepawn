#include <shell>

enum struct Num {
    int u;
}

public main()
{
    // Chained comparisons whose first operand is an expression that Check
    // rewrites (field accesses and casts produce replacement nodes). This
    // crashed in codegen when the checked first operand was threaded but not
    // re-attached to the chain, leaving the codegen-visible operand with an
    // empty value.
    Num n;
    n.u = 5;

    bool a = n.u > 1 <= 1;       // 5 > 1, then 1 <= 1  -> true
    bool b = n.u < 3 < 10;       // 5 < 3 is false      -> false
    bool c = n.u >= 5 <= 5;      // 5 >= 5, then 5 <= 5 -> true
    bool d = sizeof(Num) < 9 < 20;  // size < 9, then 9 < 20 -> true

    printnums(4, a, b, c, d);
}
