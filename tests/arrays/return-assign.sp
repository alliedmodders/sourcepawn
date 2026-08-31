#include <shell>

int[] Make(int val) {
  int[] a = new int[2];
  a[0] = val;
  return a;
}

public main() {
  int[] x = Make(10);
  int[] y = Make(20);
  x = y;
  printnum(x[0]);
}
