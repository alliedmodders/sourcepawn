#include <shell>

public main() {
  int[] x = new int[2];
  x[0] = 1;
  int[] y = x;
  x = new int[2];
  x[0] = 77;
  printnum(x[0]);
  printnum(y[0]);
}
