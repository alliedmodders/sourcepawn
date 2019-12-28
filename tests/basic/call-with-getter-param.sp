#include <shell>

methodmap Test
{
    public Test()
    {
        return 0;
    }

    property int A
    {
        public get() { return 1; }
    }
}

public void main()
{
    Test test;
    printnums(test.A, test.A);
}
