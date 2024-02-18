#include <shell>

float blah(float f) {
    return f;
}

any what(any x) {
    return blah(x);
}

public main() {
    printfloat(what(5.0));
}
