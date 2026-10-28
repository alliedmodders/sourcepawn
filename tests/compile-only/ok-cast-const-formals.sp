native void printnum(int n);
native void Poke(char[] x);
native void PokeFixed(char x[6]);
native void MutateAny(any &x);

void Print(const char[] s) {
    printnum(s[0]);
}

public void main() {
    char buf[6];
    int m = 1;
    Poke(String:"hello");
    PokeFixed(String:"hello");
    Poke(String:buf);
    MutateAny(view_as<any>(m));
    Print(String:"hi");
}
