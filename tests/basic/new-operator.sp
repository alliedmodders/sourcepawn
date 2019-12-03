#include <shell>

methodmap Clam < Handle
{
  public Clam(int c) {
    return view_as<Clam>(c);
  }
  public void dump() {
    printnum(view_as<int>(this));
  }
}

public main()
{
  Clam c = new Clam(100);
  c.dump();
}
