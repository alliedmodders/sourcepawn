enum struct ReturnVal
{
    int x;
    bool test()
    {
        // error: missing return
    }
}

public void OnPluginStart()
{
    ReturnVal v;
    v.test();
}
