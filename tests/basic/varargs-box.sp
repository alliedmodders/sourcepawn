#include <shell>

void printnums_wrapper(int prefix, ...) {
    printnums(prefix, ...);
}

public void main() {
    printnums_wrapper(5555, 1, 2, 3, 4, 5, 6);
}
