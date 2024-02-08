#include <shell>

enum struct Point {
  int x;
  int y;

  void set(int x, int y) {
    this.x = x;
    this.y = y;
  }
}

void Set(Point p, int x, int y) {
  p.x = x;
  p.y = y;
}

public main()
{
  Point p;
  p.set(5, 10);
  printnum(p.x);
  printnum(p.y);

  Set(p, 6, 11);
  printnum(p.x);
  printnum(p.y);
}
