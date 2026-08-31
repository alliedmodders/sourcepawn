// type: compiler-output
void take_ref(int8 &x) {}

public main() {
    int8 a[5];
    take_ref(a[0]);
}
