#include <shell>

#define Stringize(%0) #%0

public void main() {
    char x[] = Stringize(CRAB);
    print(x);
    print("\n");
}
