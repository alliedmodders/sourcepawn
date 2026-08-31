#include <shell>

int[] Make() {
  int[] a = new int[2];
  a[0] = 5;
  a[1] = 6;
  return a;
}

public main() {
  int[] r = Make();
  printnum(r[0]);
  printnum(r[1]);
}
