#include <shell>

int do_div(int left, int right)
{
    return left / right;
}

int do_mod(int left, int right)
{
    return left % right;
}

public main()
{
    printnum(do_div(-1, 128));
    printnum(do_mod(-1, 128));
    printnum(-1 / 128);
    printnum(-1 % 128);
}
