
typedef Callback = function void (any a);
typedef OtherCallback = function void (int64 a);

void test(int64 a) {}
void test2(any a) {}

void do_test(Callback callback) {}
void do_test_a(OtherCallback callback) {}

public void main() {
    do_test(test)
    do_test_a(test2)
}
