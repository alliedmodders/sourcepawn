#include <shell>

public void main()
{
  Test1();
  Test2();
}

void Test1() {
  int i = 0;
  do {
    if (i % 2 == 0) {
      if (i >= 1) {
        print("break\n");
        break;
      }
      else {
        print("even\n");
      }

      continue;
    }

    print("odd\n");
  }
  while (++i);
}

void Test2() {
  int i = 0;
  do {
    i++;
    printnum(i);
    continue;
  } while (i < 3);
}
