#include <shell>

enum struct Sample {
  float a;
};

methodmap Crab {
  property Sample s {
    public native get();
  }
};

public main() {
  Crab c;
  c.s;
}
