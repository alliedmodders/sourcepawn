#include <shell>

enum struct Position {
  float x;
  float y;
  float z;
}

enum struct Area {
  Position min;
  Position max;
  int tag;
}

static Area g_area = { { 1.0, 2.0, 3 }, { 4.0, 5.0, 6 }, 7 };

public main() {
  printfloat(g_area.min.x);
  printfloat(g_area.min.y);
  printfloat(g_area.min.z);
  printfloat(g_area.max.x);
  printfloat(g_area.max.y);
  printfloat(g_area.max.z);
  printnum(g_area.tag);

  Area local = { { 1.0, 2, 3.5 }, { 7 }, 9 };
  printfloat(local.min.x);
  printfloat(local.min.y);
  printfloat(local.min.z);
  printfloat(local.max.x);
  printfloat(local.max.y);
  printnum(local.tag);
}
