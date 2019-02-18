#include <shell>

enum struct Sample {
  float a;
  int x;
  int y;
  char message[32];
}

public main() {
  Sample s;
  s.a = 3.0;
  s.x = 10;
  s.y = 20;
  printfloat(s.a);
  printnum(s.x);
  printnum(s.y);
}
