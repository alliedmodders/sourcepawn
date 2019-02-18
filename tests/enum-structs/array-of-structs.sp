#include <shell>

enum struct Sample {
  int x;
  int y;
  int data[5];
}

public main() {
  Sample s[5];
  s[3].data[2] = 10;
  printnum(s[3].data[2]);
}
