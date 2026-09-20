public int64 AddOverflow()
{
    return 9223372036854775807 + 1;
}

public int64 MulOverflow()
{
    return 4294967296 * 4294967296;
}

public int64 NegAddOverflow()
{
    return (~9223372036854775807) + (-1);
}
