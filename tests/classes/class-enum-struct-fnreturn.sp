#include <shell>

enum struct Point {
    float x;
    float y;
}

Point MakePoint() {
    Point p;
    p.x = 1.0;
    p.y = 2.0;
    return p;
}

public void main() {
    Point p = MakePoint();
    printfloat(p.x);
    printfloat(p.y);
}
