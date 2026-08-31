enum struct Point {
    int x;
    int y;
}

class C {
    property Point Prop {
        get() { return Point:{0, 0}; }
    }
}

public void main() {}
