void MutateParam(const char[] s) {
    s[0] = 'Z';
    s[0]++;
    s[0] += 1;
    --s[0];
}

void MutateLiteral(bool cond) {
    "hello"[0] = 'Z';
    "hello"[0]++;
    (cond ? "hello" : "world")[0] = 'Z';
}

public void main() {
    MutateParam("hello");
    MutateLiteral(true);
}
