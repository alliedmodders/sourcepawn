#include <shell>

void f(int a, int b, int c) {
  printnum(a);
  printnum(b);
  printnum(c);
}

public main() {
  f(12, .c = 9, .b = 10);
}
