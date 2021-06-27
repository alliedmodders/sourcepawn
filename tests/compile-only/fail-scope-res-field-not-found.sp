native void PrintToServer(const char[] a, int b);

enum struct A
{
    int a;
}

public void OnPluginStart()
{
    PrintToServer("%d", A::b);
}
