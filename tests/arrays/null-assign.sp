#include <shell>

public main() {
  int[] x = new int[2];
  x[0] = 42;
  printnum(x[0]);
  x = null;
  printnum(x == null ? 1 : 0);
}
