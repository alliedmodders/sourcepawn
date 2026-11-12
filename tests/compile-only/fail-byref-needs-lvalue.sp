void SetVal(int &out) {
    out = 1;
}

public void main() {
    bool c = true;
    int a = 1;
    int b = 2;
    SetVal(c ? a : b);
    SetVal(a + b);
    SetVal(3);
}
