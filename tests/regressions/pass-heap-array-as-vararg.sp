#include <shell>

void Func(char[] fmt, any ...)
{
    printf(fmt, ...);
}

public void main()
{
    char[] fmt = "(value1 %d) (str %s) (value2 %d)";
    char[] str = "some string";
    int value1 = 1234567;
    int value2 = 7654321;

    Func(fmt, value1, str, value2);
}
