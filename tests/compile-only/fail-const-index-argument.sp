native void PokeFixed(char x[6]);
native void PokeSlice(char[] x);
native void Mutate(int &x);

public const char g2[2][6] = { "hello", "world" };
public const char g3[2][2][6] = { { "hello", "world" }, { "abcde", "fghij" } };
public const int ga[4] = { 1, 2, 3, 4 };

void PassConst() {
    PokeFixed(g2[0]);
    PokeSlice(g3[0][0]);
    Mutate(ga[0]);
}

public void main() {
    PassConst();
}
