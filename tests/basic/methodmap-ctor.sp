#include <shell>

methodmap Clam {
  public Clam(int& a) {
    return view_as<Clam>(a);
  }
  public void dump() {
    printnum(view_as<int>(this));
  }
}

public main() {
  int a = 10;
  Clam clam = Clam(a);
  clam.dump();
}
