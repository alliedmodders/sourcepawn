#include <shell>

int[] x = null;
int[] g1 = null;
int[] g2 = new int[5];
int[] g3 = g2;

public main() {
  printnum(x == null ? 1 : 0);
  printnum(g1 == null ? 1 : 0);
  printnum(g2 == null ? 1 : 0);
  printnum(g3 == null ? 1 : 0);
  printnum(g2 == g3 ? 1 : 0);
  g2[0] = 42;
  printnum(g3[0]);
}
