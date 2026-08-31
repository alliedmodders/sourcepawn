// type: compiler-output
float g_float;
int8 g_i8;

void test_widen() {
    view_as<int8>(g_float);   // float (4) -> int8 (1)
    view_as<int64>(g_float);  // float (4) -> int64 (8)
}

void test_shrink() {
    view_as<float>(g_i8);     // int8 (1) -> float (4)
}
