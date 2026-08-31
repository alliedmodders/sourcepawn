#include <shell>

class Player {
    int id;
}

typedef Callback = () -> int;

Callback GetObjectCallback(Player p) {
    return function () -> int { return p.id; };
}

Callback GetSharedObjectCallback(Player initial_p) {
    shared Player p = initial_p;
    let cb = function () -> int { return p.id; };
    // Reassign the captured shared object reference
    p = new Player();
    p.id = 50;
    return cb;
}

public void main() {
    Player p1 = new Player();
    p1.id = 42;
    let cb1 = GetObjectCallback(p1);
    p1.id = 100;
    printnum(cb1()); // Object is reference type; by-value capture stores the reference

    Player p2 = new Player();
    p2.id = 20;
    let cb2 = GetSharedObjectCallback(p2);
    printnum(cb2()); // Shared capture sees the reassigned object reference
}
