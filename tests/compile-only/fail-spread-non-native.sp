void MyVariadic(any...) {
}

void Caller(any...) {
    MyVariadic(...);
}

public void main() {
    Caller(1, 2, 3);
}
