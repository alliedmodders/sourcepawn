native void Poke(char[] x);
native void printnum(int n);

char g_buf[6] = "hello";
public const char g_fixed[6] = "hello";

void DefaultMutable(char[] x = g_buf) {
    Poke(x);
}

char[] ReturnLiteral() {
    return "hello";
}

char[] ReturnMutable(char[] s) {
    return s;
}

void InitLiteral() {
    char[] alias = "hello";
    Poke(alias);
}

void AssignLiteral() {
    char[] u = new char[6];
    u = "hello";
    Poke(u);
}

void CopyIntoFixed() {
    char buf[6];
    buf = g_fixed;
    char buf2[6];
    buf2 = "hello";
    printnum(buf[0] + buf2[0]);
}

void LetConstReads(const char[] s, bool cond) {
    let u = s;
    printnum(u[0]);
    let t = cond ? "hello" : "world";
    printnum(t[0]);
}

public void main() {
    DefaultMutable();
    Poke(ReturnLiteral());
    char[] m = new char[6];
    Poke(ReturnMutable(m));
    InitLiteral();
    AssignLiteral();
    CopyIntoFixed();
    LetConstReads("hello", true);
}
