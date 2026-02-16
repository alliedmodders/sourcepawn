#include <shell>

char myStr[][] =  { "str1", "str2" };

enum struct myStruct
{
    int str[sizeof(myStr)];
}

public main()
{
    printnum(sizeof(myStruct));
}
