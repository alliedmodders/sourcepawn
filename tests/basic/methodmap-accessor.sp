#include <shell>

int gVal = 0;

methodmap Clam {
  public Clam(int a) { return view_as<Clam>(a); }

  property int value {
    public get() { return gVal; }
    public set(int v) { gVal = v; }
  }
};

public main()
{
  Clam c = Clam(0);
  printnum(c.value);
  printnum(c.value++);
  printnum(c.value--);
  printnum(++c.value);
  printnum(--c.value);
  printnum(c.value);
}
