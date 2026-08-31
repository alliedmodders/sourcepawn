#include <shell>

public main()
{
  intptr a = 20;
  intptr b = ~a;
  intptr c = -a;
  printnumptr(b);
  printnumptr(c);
  printnumptr(++a);
  printnumptr(a--);
  printnumptr(a);
}
