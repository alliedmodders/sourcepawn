// type: compiler-output
void test_const_arity() {
    const int16 A = 30000;
    const int16 B = 30000;
    int16 x = A + B;        // 60000 -> out of range
}

void test_max_plus_one() {
    const int16 M = 32767;
    const int16 O = 1;
    int16 y = M + O;        // 32768 -> out of range
}
