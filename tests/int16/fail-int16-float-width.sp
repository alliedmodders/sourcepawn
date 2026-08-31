// type: compiler-output
float g_float;
int16 g_i16;

void test_widen() {
    view_as<int16>(g_float);   // float (4) -> int16 (2)
    view_as<int64>(g_float);   // float (4) -> int64 (8)
}

void test_shrink() {
    view_as<float>(g_i16);     // int16 (2) -> float (4)
}
