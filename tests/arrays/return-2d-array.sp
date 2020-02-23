#include <shell>

char[][] blah()
{
    char x[][] = {
         "hellooo\n",
         "goodbye\n",
    };
    return x;
}

public main()
{
    print(blah()[1]);
}
