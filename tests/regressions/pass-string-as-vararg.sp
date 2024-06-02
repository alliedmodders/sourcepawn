#include <shell>

int TestNumber = 0;

void test(const char[] title) {
    TestNumber++;
    printf("%d: %s\n", TestNumber, title);
}

public main() {
    test("Ham");
    printf("%s\n", "TEST");
}

