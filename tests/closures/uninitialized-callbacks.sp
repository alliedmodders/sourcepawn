#include <shell>

typedef Callback = () -> int;

public void main() {
    Callback me;
    shared Callback shared_me;
    shared_me = function() -> int {
        return 1;
    };
    let reader = function() -> int {
        return shared_me == null ? 0 : 1;
    };
    printnum(me == null ? 0 : 1);
    printnum(reader());
    printnum(shared_me());
}
