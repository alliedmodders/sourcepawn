#include <shell>

public main() {
  int[][] x = new int[2][];
  x[0] = new int[3];
  x[1] = new int[3];
  x[0][0] = 1;
  x[0][1] = 2;
  x[1][0] = 3;
  printnum(x[0][0]);
  printnum(x[0][1]);
  printnum(x[1][0]);
}
