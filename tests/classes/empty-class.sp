#include <shell>

class Empty {}

class EmptyMethod {
    void Ping() {
        print("ping\n");
    }
}

class Holder {
    Empty empty;
    int after;

    Empty GetEmpty() {
        return this.empty;
    }
}

public void main() {
    Empty e = new Empty();
    if (e != null)
        printnum(1);

    EmptyMethod m = new EmptyMethod();
    m.Ping();

    Holder h = new Holder();
    h.empty = new Empty();
    h.after = 42;
    printnum(h.after);
    if (h.GetEmpty() != null)
        printnum(2);
}
