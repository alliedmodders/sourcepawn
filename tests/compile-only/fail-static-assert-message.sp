enum
{
    TEST_1,
    TEST_2,
    TEST_MAX
}

static_assert(TEST_MAX == TEST_1, "oh no this failed how come")

public main()
{
    static_assert(TEST_MAX == TEST_1, "oh no this failed how come 2")
}
