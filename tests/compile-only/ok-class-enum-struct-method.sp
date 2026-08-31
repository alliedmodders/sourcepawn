enum struct Point {
    float x;
    float y;
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
    s.SetOrigin({1.0, 2.0});
}
