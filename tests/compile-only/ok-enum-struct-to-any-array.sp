// warnings_are_errors: true
#include <shell>

dostuff(any[] array) {
  array[0] = 0;
}

enum struct X {
  int inner[5];
}

public main()
{
  X x;
  dostuff(x);
}
