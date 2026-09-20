#include <shell>

typedef Fn = () -> int;
typedef UnaryFn = (int x) -> int;

class Handler {
    Fn callback;
    UnaryFn transform;
}

int AddOne(int x) {
    return x + 1;
}

public void main() {
    Handler h = new Handler();
    h.callback = function () -> int { return 5; };
    printnum(h.callback());

    Fn g = h.callback;
    printnum(g());

    Handler h2 = new Handler();
    h2.callback = g;
    printnum(h2.callback());

    h.transform = AddOne;
    printnum(h.transform(41));
}
