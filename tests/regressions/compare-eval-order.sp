#include <shell>

public void main()
{
    int i = 0;
    // Expected 0 >= 10
    // 1.11 Real 10 >= 10
    if (i >= (i = 10))
    {
        print("1, 1\n");
    }
    else
    {
        print("1, 0\n");
    }
    i = 0;
    printnums(2, i >= (i = 10));
}
