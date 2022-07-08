enum
{
    TEST_1,
    TEST_2,
    TEST_MAX
}

static_assert(TEST_MAX == TEST_MAX)

public main()
{
    static_assert(TEST_MAX == TEST_MAX)
}
