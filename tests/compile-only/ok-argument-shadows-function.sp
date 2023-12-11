// warnings_are_errors: true

int min(int a, int b) {
    return a < b ? a : b;
}

stock void test(float min, float max) {
#pragma unused min
#pragma unused max
}

public main() {
    return min(2, 3);
}
