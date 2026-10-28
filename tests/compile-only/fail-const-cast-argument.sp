native void Poke(char[] x);
native void MutateAny(any &x);

public const char g_str[6] = "hello";

void F(const int n) {
    Poke(String:g_str);
    MutateAny(view_as<any>(n));
}

public void main() {
    int m = 1;
    F(m);
}
