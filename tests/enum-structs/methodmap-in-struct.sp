#include <shell>

methodmap T {
  public void print() {
    printnum(view_as<int>(this));
  }
}

enum struct X {
  T t;
}

public main()
{
  T t = view_as<T>(5);
  t.print();

  X x;
  x.t = t;
  x.t.print();
}
