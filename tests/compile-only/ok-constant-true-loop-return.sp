// Regression test for constant-true loop conditions with returning bodies.
// These used to emit a jump past the end of the function, failing the
// verifier with "Instruction contained invalid parameter".
typedef Cb = ()->int;

public void main() {
    Cb c = function () -> int { while (true) { return 5; } };
}

public void do_while_true() {
    int x = 5;
    do {
        if (x != 0)
            return;
    } while (true);
}

public int while_true() {
    while (true) {
        return 5;
    }
}
