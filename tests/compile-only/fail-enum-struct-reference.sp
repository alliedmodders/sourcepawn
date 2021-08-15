#include <shell>

enum struct Point {
  int x;
}

void Crab(Point& p) {
  p.x = 10;
}

public main()
{
  Point p;
  Crab(p);
}
