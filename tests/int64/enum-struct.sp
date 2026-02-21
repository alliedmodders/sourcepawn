native void printnum64(int64 a);

enum struct Clams {
    int64 a;
    int64 b;
}

public void main() {
    Clams clam;
    clam.a = 123456789123456789;
    clam.b = 234567891234567891;

    printnum64(clam.a);
    printnum64(clam.b);
}
