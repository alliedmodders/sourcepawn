// type: compiler-output
void test_const_arity() {
    const int8 A = 100;
    const int8 B = 50;
    int8 x = A + B;        // 150 -> out of range
}

void test_max_plus_one() {
    const int8 M = 127;
    const int8 O = 1;
    int8 y = M + O;        // 128 -> out of range
}
