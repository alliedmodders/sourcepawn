#include <shell>

enum struct Sample {
  int x;
  int y;
  int data[5];
}

public main() {
  Sample[] s = new Sample[10];

  s[1].x = 12;
  s[2].data[3] = 500;
  printnum(s[1].x);
  printnum(s[2].data[3]);
}
