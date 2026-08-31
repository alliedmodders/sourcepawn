#include <shell>

enum struct Point {
    int x;
    int y;
}

class Shape {
    Point origin;

    void SetOrigin(Point p) {
        this.origin = p;
    }

    Point GetOrigin() {
        return this.origin;
    }
}

public void main() {
    Shape s = new Shape();
    s.SetOrigin({10, 20});
    Point p = s.GetOrigin();
    printnum(p.x);
    printnum(p.y);
}
