#include <shell>

typedef Callback1 = ();
typedef Callback2 = () -> int;

public void Invoke1(Callback1 callback) {
    callback();
}

public void Invoke2(Callback2 callback) {
    printnum(callback());
}

public void main() {
    function inner() { print("inner\n"); }
    let other_inner = function () -> int { return 777; }
    Invoke1(inner);
    Invoke2(other_inner);
}
