enum struct STest
{
   int mSample;

    int GetSize()
    {
        return sizeof(STest);
    }
}

public main()
{
    STest test;
    test.GetSize();
}
