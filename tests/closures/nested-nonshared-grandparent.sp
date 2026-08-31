#include <shell>

typedef Callback = () -> int;

// Regression: a nested closure chain must work for non-shared captures
// as well as shared ones. Each closure copies its parents' captured
// values into its own upvar slots at construction time, so intermediate
// closures need to forward ancestor upvars through their own upvar chain.
Callback GetOuter() {
    int a = 1;
    Callback outer = function () -> int {
        int b = 2;
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
