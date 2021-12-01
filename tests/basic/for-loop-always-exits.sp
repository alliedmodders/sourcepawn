#include <shell>

public main()
{
  for (int i = 0; i < 3; i++) {
    if (i != 3) {
      printnum(i);
      continue;
    }

    break;
  }
}
