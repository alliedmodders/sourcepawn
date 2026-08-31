#include <shell>

enum struct Vector {
    int x;
    int y;
    int z;
}

typedef Callback = () -> int;

Callback GetEnumStructCallback(Vector v) {
    return function () -> int { return v.x + v.y + v.z; };
}

Callback GetSharedEnumStructCallback(Vector in_v) {
    let shared v = in_v;
    let cb = function () -> int { return v.x + v.y + v.z; };
    v.x = 10;
    v.y = 20;
    v.z = 30;
    return cb;
}

public void main() {
    Vector v1;
    v1.x = 1; v1.y = 2; v1.z = 3;
    let cb1 = GetEnumStructCallback(v1);
    v1.x = 100; // Mutating original shouldn't affect the captured copy
    printnum(cb1());
    
    Vector v2;
    v2.x = 1; v2.y = 2; v2.z = 3;
    let cb2 = GetSharedEnumStructCallback(v2);
    printnum(cb2()); // Mutating original affects shared capture
}
