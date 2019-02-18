#include <shell>

enum struct Inner {
  int x;
  char message[32];
}

enum struct Middle {
  Inner inner;
}

enum struct Sample {
  float a;
  Middle m;
  int y;
}

public main() {
  Sample s;
  s.a = 30.0;
  s.m.inner.x = 10;
  s.y = 50;
  printfloat(s.a);
  printnum(s.m.inner.x);
  printnum(s.y);
}
