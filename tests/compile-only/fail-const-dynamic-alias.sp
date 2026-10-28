native void Poke(char[] x);

public const char g_const[6] = "hello";

void DefaultAlias(char[] x = g_const) {
    Poke(x);
}

char[] ReturnAlias(const char[] s) {
    return s;
}

void InitAlias(const char[] s) {
    char[] alias = s;
}

void AssignAlias(const char[] s) {
    char[] u = new char[6];
    u = s;
}

void LetTernary(bool cond) {
    let s = cond ? "hello" : "world";
    Poke(s);
    s[0] = 'Z';
}

void LetSymbol(const char[] s) {
    let u = s;
    Poke(u);
}

public void main() {
    DefaultAlias();
    InitAlias("hello");
    AssignAlias("hello");
    LetTernary(true);
    LetSymbol("hi");
}
