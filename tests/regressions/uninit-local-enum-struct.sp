#include <shell>

// Regression: uninitialized local enum structs (and arrays of them) were
// not zero-initialized at their point of declaration, so stale frame data
// persisted across executions of the declaration.

enum struct Vec {
    int x;
    int y;
    char name[4];
}

public void main() {
    for (int i = 0; i < 2; i++) {
        Vec v;
        Vec vs[3];
        if (i == 0) {
            v.x = 5; v.y = 6; v.name[0] = 'z';
            vs[2].y = 11;
        }
        printnum(v.x + v.y + v.name[0] + vs[0].x + vs[1].y + vs[2].x + vs[2].y);
    }
}
