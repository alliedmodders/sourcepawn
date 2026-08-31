native void printnum(int a);

// Tests for NaN handling in double precision.
//
// SourcePawn excludes NaN from truthiness: the bitwise test pattern
// (d != 0.0 && !IsNaN(d)) is used in both the interpreter and JIT.
// IEEE 754 unordered comparisons also apply: any compare against NaN
// returns false except !=, which returns true.

public main() {
    double nan = 0.0d / 0.0d;

    // Truthiness (exercises LL_TEST_F64 via OP_TEST).
    if (nan)
        printnum(1);
    else
        printnum(0);

    if (!nan)
        printnum(1);
    else
        printnum(0);

    // Unordered comparisons against 0.0 and against self.
    printnum(nan == 0.0d);
    printnum(nan == 3.14d);
    printnum(nan == nan);

    printnum(nan != 0.0d);
    printnum(nan != 3.14d);
    printnum(nan != nan);

    printnum(nan < 0.0d);
    printnum(nan < 3.14d);
    printnum(nan < nan);

    printnum(nan <= 0.0d);
    printnum(nan <= 3.14d);
    printnum(nan <= nan);

    printnum(nan > 0.0d);
    printnum(nan > 3.14d);
    printnum(nan > nan);

    printnum(nan >= 0.0d);
    printnum(nan >= 3.14d);
    printnum(nan >= nan);
}
