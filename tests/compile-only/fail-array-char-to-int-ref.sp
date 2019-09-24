#include <shell>

void blah(int& a) {
  printnum(a);
}

public main() {
  char s[] = "Hello!";
  blah(s[1]);
}
