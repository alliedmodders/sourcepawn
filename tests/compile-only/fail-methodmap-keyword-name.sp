methodmap break TestingA < StringMap
{
    public TestingA()
    {
        return view_as<TestingA>(new StringMap());
    }

    public void Test()
    {
        this.SetValue("test", 0);
    }
}

methodmap TestingB < TestingA
{
    public TestingB()
    {
        return view_as<TestingB>(new TestingA());
    }
}

TestingB g_Test = null;

public void OnPluginStart()
{
    g_Test = ReturnB();
}

TestingB ReturnB()
{
    return new TestingB();
}

public void OnConfigsExecuted()
{
    g_Test.Test();
}
