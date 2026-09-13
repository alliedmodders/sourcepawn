// Valid subtractions with opposite-signed operands must fold cleanly.
// Regression: the MSVC CheckedSub fallback compared the wrapped result's
// sign against the wrong operand, rejecting these as overflow.
public int NegMinusPos()
{
    return -1 - 1;
}

public int PosMinusNeg()
{
    return 5 - (-3);
}

public int64 Int64NegMinusPos()
{
    return -9223372036854775807 - 1;
}

public int64 Int64PosMinusNeg()
{
    return 9223372036854775806 - (-1);
}
