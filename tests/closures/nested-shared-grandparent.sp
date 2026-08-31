#include <shell>

typedef Callback = () -> int;

// Regression: a nested closure must capture shared variables from a
// grandparent's scope (not just its immediate parent's). Without proper
// chain propagation the bytecode fails verification.
Callback GetOuter() {
    shared int a = 1;
    Callback outer = function () -> int {
        shared int b = 2;
        Callback inner = function () -> int {
            return a + b;
        };
        return inner();
    };
    return outer;
}

public void main() {
    printnum(GetOuter()());
}
