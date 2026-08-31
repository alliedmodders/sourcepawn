#include <shell>

enum struct Point {
    float x;
    float y;
}

class Shape {
    Point origin;

    void SetOrigin(Point p) {
        this.origin = p;
    }

    void PrintOrigin() {
        printfloat(this.origin.x);
        printfloat(this.origin.y);
    }
}

public void main() {
    Shape s = new Shape();
    s.SetOrigin({1.5, 2.5});
    s.PrintOrigin();
}
