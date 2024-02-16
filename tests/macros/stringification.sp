#include <shell>

#define TO_STR(%0)  #%0

#define C1          ab
#define C2          "ab"
#define C3()        cd
#define C4(%0)      %0

public main() {
    print(TO_STR(C1));
    print("\n");
    
    print(TO_STR(C2));
    print("\n");
    
    print(TO_STR(C3));
    print("\n");
    
    print(TO_STR(C3()));
    print("\n");
    
    print(TO_STR(C4(15)));
    print("\n");
    
    print(TO_STR(UNDEFINED));
}