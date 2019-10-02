#include <shell>

int f(int c[3]) {
  printnum(c[0]);
  printnum(c[1]);
  printnum(c[2]);
}

public main() {
  f({9, 5, 2});
}
