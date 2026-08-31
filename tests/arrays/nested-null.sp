#include <shell>

public main() {
  int[][] x = new int[2][];
  x[0] = new int[3];
  x[0][0] = 42;
  printnum(x[0][0]);
  x[0] = null;
  printnum(x[0] == null ? 1 : 0);
  x[1] = new int[1];
  x[1][0] = 7;
  printnum(x[1][0]);
}
