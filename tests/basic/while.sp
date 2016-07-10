#include <shell>

int sFirst = 1;
int sLast = 1;

public main()
{
  while (sFirst == sLast) {
    print("ok\n");
    if (sFirst > 0 && sLast > 0 && donothing())
      break;
  }
}
