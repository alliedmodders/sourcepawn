#include <shell>
#include "file-const-include.inc"

public main() {
    print(__FILE_NAME__);
    print("\n");
    print(FILE);
    print("\n");
    PrintIncludeName();
    print("\n");
    PrintIncludeName2();
    print("\n");

    printnum(StringLength(__FILE_PATH__) > StringLength(__FILE_NAME__));

    char buffer[512];
    GetIncludeName(buffer, sizeof(buffer));
    print(buffer);
    print("\n");

    printnum(StringEquals(__FILE_NAME__, FILE));
    printnum(StringEquals(__FILE_NAME__, buffer));

    GetIncludePath(buffer, sizeof(buffer));
    printnum(StringEquals(__FILE_PATH__, PATH));
    printnum(StringEquals(__FILE_PATH__, buffer));
}

bool StringEquals(char[] leftString, char[] rightString) {
    for(int i = 0;; i++)
    {
        if(leftString[i] != rightString[i])
            return false;

        if(leftString[i] == 0)
            return true;
    }
}

int StringLength(char[] string) {
    int length = 0;
    for(;;)
    {
        if(string[length++] == 0)
            return length;
    }
}