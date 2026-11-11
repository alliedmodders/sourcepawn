enum struct Foo
{
    int x;

    // Regression test for issue 760: enum struct methods must be callable
    // on a const enum struct, since there is no way to declare a const
    // method.
    void DoNothing()
    {
    }

    int GetX()
    {
        return this.x;
    }
}

int DoStuff(const Foo b)
{
    b.DoNothing();
    return b.GetX();
}

public void main()
{
    Foo bar;
    DoStuff(bar);
}
