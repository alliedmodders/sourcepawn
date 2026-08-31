#include <shell>

public main() {
  int[] x = new int[3];
  x[0] = 10;
  x[1] = 20;
  x[2] = 30;

  int[] y = x;
  printnum(y[0]);
  printnum(y[1]);
  printnum(y[2]);

  y[1] = 99;
  printnum(x[1]);
}
