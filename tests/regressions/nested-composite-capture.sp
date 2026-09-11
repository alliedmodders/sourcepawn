#include <shell>

// Regression: captured local arrays and enum structs (composites) could not
// be propagated through an intermediate closure. The closure-creation path
// emitted load.upvar instead of addr.upvar for forwarded composites, which
// the verifier rejected with "Invalid parameter or parameter type".

enum struct Counter {
    int value;
}

typedef Callback = () -> int;
typedef CallbackFactory = () -> Callback;
typedef VoidFn = () -> void;

CallbackFactory GetFactory() {
    int arr[2];
    Counter c;
    arr[0] = 40;
    return function () -> Callback {
        return function () -> int { return arr[0] + arr[1] + c.value; };
    };
}

public void main() {
    int arr[2];
    Counter c;
    arr[0] = 40;
    c.value = 2;

    VoidFn mid = function () -> void {
        VoidFn inner = function () -> void {
            printnum(arr[0] + arr[1] + c.value);
        };
        inner();
    };
    mid();

    // Closures over uninitialized composites (zeroed at declaration) that
    // escape the declaring frame, with an intermediate closure re-forwarding
    // them.
    let factory = GetFactory();
    let cb = factory();
    printnum(cb());
}
