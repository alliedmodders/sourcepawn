#include <shell>

public main() {
  for (int i = 0; i < 10; i++) {
    if (i % 2 == 0)
      continue;
    printnum(i);
  }
}
