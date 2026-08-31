#include <shell>

class Point {
    float x;
    float y;

    property float X {
        get() { return this.x; }
        set(float v) { this.x = v; }
    }

    property float Y {
        get() { return this.y; }
        set(float v) { this.y = v; }
    }

    void PrintX() {
        printfloat(this.x);
    }

    void PrintY() {
        printfloat(this.y);
    }
}

class Line {
    Point start;
    Point end;

    property Point Start {
        get() { return this.start; }
        set(Point p) { this.start = p; }
    }

    property Point End {
        get() { return this.end; }
        set(Point p) { this.end = p; }
    }
}

public void main() {
    Line line = new Line();
    Point p1 = new Point();
    p1.X = 1.5;
    p1.Y = 2.5;

    Point p2 = new Point();
    p2.X = 3.0;
    p2.Y = 4.0;

    line.Start = p1;
    line.End = p2;

    line.Start.PrintX();
    line.Start.PrintY();
    line.End.PrintX();
    line.End.PrintY();
}
