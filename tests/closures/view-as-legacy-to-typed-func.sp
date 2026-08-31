#include <shell>

typedef LegacyFunc = function void(int x);
typedef NewFunc = (int x) -> void;

void g(int x) {
    printnum(x);
}

public void main() {
    LegacyFunc lf = g;
    NewFunc nf = view_as<NewFunc>(lf);
    nf(42);
}
