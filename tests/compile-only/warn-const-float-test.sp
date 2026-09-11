// A float constant in a test expression used to assert in
// ConstVal::get_i32() (via Semantics::AnalyzeForTest). The condition should
// be checked using the constant's actual type, emitting warning 205/206.
public main() {
    if (1.5)
        return 1;
    if (0.0)
        return 2;
    int x = (1.5 ? 3 : 4);
    return x;
}
