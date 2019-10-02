#include <shell>

void whatever() {
  print("whatever\n");
}

public main() {
  int blah = 10;
  int x = (1, 2, 3, whatever(), blah);
  printnum(x);
}
