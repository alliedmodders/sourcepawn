// Regression test for a compiler assertion.
//
// A bogus directive mid-line truncates the first function's body. Stale
// lexer state used to poison parsing of the second function, tripping an
// assertion. Compilation must fail, but must not crash.
public void main() {
    int x;
    x = 1;
    x#pragma_float);
}

void other() {
    int y;
    y = 2;
}
