#include <shell>

public main()
{
    // Chained comparisons where the operands have a wide type (int64 or float)
    // would previously trip an assertion in ExprVal::const_i32() because the
    // constant-folding path in CheckChainedCompareExpr only knew how to read
    // cell-sized constants.
    bool a = 5000000000 < 9000000000 < 12000000000;
    bool b = 5000000000 > 9000000000 > 12000000000;
    bool c = 1.0 < 2.0 < 3.0;
    bool d = 3.0 < 2.0 < 1.0;
    printnums(4, a, b, c, d);
}
