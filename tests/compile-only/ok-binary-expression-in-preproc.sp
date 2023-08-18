// warnings_are_errors: true
#include <shell>

#define A 1
#define B 1

public void main()
{
    #if defined A && defined B
    print("AND OK\n");
    #endif
    
    #if defined A || defined B
    print("OR OK\n");
    #endif
}
