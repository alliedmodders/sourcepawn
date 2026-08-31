#include <shell>

typedef Callback = () -> int;
typedef Mutator = () -> void;

// Closure captures a by-ref parameter and reads it.
Callback GetReader(int& x) {
    return function () -> int { return x; };
}

// Closure captures a by-ref parameter and writes to it.
Mutator GetMutator(int& x) {
    return function () -> void { x += 100; };
}

// Closure captures a by-ref parameter, reads and writes.
Callback GetReadWriter(int& x) {
    return function () -> int {
        int old = x;
        x = old * 10;
        return old;
    };
}

// Closure captures two by-ref parameters.
Callback GetSwapTest(int& a, int& b) {
    return function () -> int {
        int tmp = a;
        a = b;
        b = tmp;
        return a;
    };
}

public void main() {
    // Test 1: Read captured by-ref parameter.
    int v1 = 42;
    let reader = GetReader(v1);
    printnum(reader()); // 42
    v1 = 99;
    printnum(reader()); // 42 - captured by value

    // Test 2: Write through captured by-ref parameter.
    int v2 = 10;
    let mutator = GetMutator(v2);
    mutator();
    printnum(v2); // 10 - captured by value, original variable unmodified

    // Test 3: Read and write through captured by-ref parameter.
    int v3 = 7;
    let rw = GetReadWriter(v3);
    printnum(rw());  // 7
    printnum(v3);    // 7
    printnum(rw());  // 70
    printnum(v3);    // 7

    // Test 4: Capture two by-ref parameters.
    int a = 1, b = 2;
    let swap = GetSwapTest(a, b);
    printnum(swap()); // 2 (a now holds b's value)
    printnum(a);      // 1
    printnum(b);      // 2
}
