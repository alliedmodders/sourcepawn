// returnCode: 1
#include <shell>

public main()
{
  int x[] = {1, 2, 3, 4};
  int n = -1;
  bad(x, n);
}

void bad(int[] x, int n)
{
  printnum(x[n]);
}
