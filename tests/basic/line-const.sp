// compiler: spcomp
#include <shell>

#define linedefine __LINE__
#define linemacro() printnum(__LINE__)

public main() {
    int iLine = linedefine;

    printnum(
        linedefine
    );
    printnum(iLine);
    linemacro(); linemacro();
}
