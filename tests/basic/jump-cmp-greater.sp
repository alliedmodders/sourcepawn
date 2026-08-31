#include <shell>

public main() {
  testJumpGreater(10, 5);
  testJumpGreater(5, 10);
  testJumpGreater(5, 5);
}

void testJumpGreater(int a, int b) {
  if (a > b || a == -999) {
    print("greater\n");
  } else {
    print("not greater\n");
  }
}
