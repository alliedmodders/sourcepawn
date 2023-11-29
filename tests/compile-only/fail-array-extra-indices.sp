enum struct A
{
    int b[1];
}

public void main()
{
    A a;
    a.b[0] = 0;
    a.b[0][0] = 0;
    a.b[0][0][0] = 0;
    a.b[0][0][0][0] = 0;
}
