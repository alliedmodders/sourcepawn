#include <shell>

methodmap X {
  public void dump() {
    printnum(view_as<int>(this));
  }
}

enum struct Point {
  int x;
  int y;

  X set(int x, int y) {
    this.x = x;
    this.y = y;
    return view_as<X>(this.y);
  }
}

public main()
{
  Point p;
  p.set(5, 7).dump();
}
