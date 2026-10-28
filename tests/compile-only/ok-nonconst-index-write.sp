native void printnum(int n);
native void PokeFixed(char x[6]);
native void PokeSlice(char[] x);
native void Mutate(int &x);

char g2[2][6] = { "hello", "world" };
char g3[2][2][6] = { { "hello", "world" }, { "abcde", "fghij" } };
int ga[4] = { 1, 2, 3, 4 };

void MutateParam(char[] s) {
    s[0] = 'Z';
    s[0]++;
    s[0] += 1;
    --s[0];
}

void PassMutable() {
    PokeFixed(g2[0]);
    PokeFixed("hello");
    PokeSlice(g3[0][0]);
    Mutate(ga[0]);
}

public void main() {
    char buf[6];
    MutateParam(buf);
    PassMutable();
    printnum("hello"[0]);
}
