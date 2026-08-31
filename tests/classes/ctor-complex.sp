#include <shell>

class Point {
    float x;
    float y;

    Point(float x, float y) {
        this.x = x;
        this.y = y;
    }

    void Print() {
        printfloat(this.x);
        printfloat(this.y);
    }
}

class Rect {
    Point topLeft;
    Point bottomRight;

    Rect(Point topLeft, Point bottomRight) {
        this.topLeft = topLeft;
        this.bottomRight = bottomRight;
    }

    void Print() {
        this.topLeft.Print();
        this.bottomRight.Print();
    }
}

public void main() {
    Point a = new Point(10.0, 20.0);
    Point b = new Point(30.0, 40.0);
    Rect r = new Rect(a, b);
    r.Print();
}
