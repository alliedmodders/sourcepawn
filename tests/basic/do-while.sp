#include <shell>

public void main()
{
  int i;

  i = 0;
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

  i = 0; 
  do { 
    i++;
    printnum(i);
    continue; 
  } while (i < 3);
}
