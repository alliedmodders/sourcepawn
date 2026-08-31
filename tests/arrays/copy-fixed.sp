#include <shell>

public main() {
  int a[3][4];
  int b[3][4];
  a[0][0] = 1;
  a[0][1] = 2;
  b[1] = a[0];
  printnum(b[1][0]);
  printnum(b[1][1]);
  a[0][0] = 99;
  printnum(b[1][0]);
}
