#include <shell>

typedef Callback = () -> int;

public main() {
    Callback cb = view_as<Callback>(0);
    printnum(cb());
    return 0;
}
