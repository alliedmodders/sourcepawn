#include <shell>

public main() {
  int[][] x = new int[2][];
  int[] inner = new int[2];
  inner[0] = 50;
  x[0] = inner;
  inner[0] = 99;
  printnum(x[0][0]);
  x[0] = new int[2];
  x[0][0] = 0;
  printnum(inner[0]);
}
