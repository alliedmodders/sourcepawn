#include <shell>

public main() {
  int[] x = new int[2];
  if (x != null)
    printnum(1);
  else
    printnum(0);
  x = null;
  if (x == null)
    printnum(2);
  else
    printnum(3);
}
