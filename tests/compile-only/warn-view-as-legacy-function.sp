#include <shell>

typedef Callback = function int ();

public main() {
    Callback cb = view_as<Callback>(0);
    return 0;
}
