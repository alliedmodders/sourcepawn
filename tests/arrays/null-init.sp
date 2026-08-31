#include <shell>

public main() {
  int[] x = null;
  printnum(x == null ? 1 : 0);
  x = new int[2];
  printnum(x == null ? 1 : 0);
}
