#include <shell>

enum struct X {
  int a;
  float x;
  float y;
  char message[20];
};

public main() {
  X x;
  int a = 2;
  printnum(x[0]);
  printnum(x[a]);
}
