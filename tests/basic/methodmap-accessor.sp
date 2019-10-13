#include <shell>

int gVal = 0;

methodmap Clam {
  public Clam(int a) { return view_as<Clam>(a); }

  property int value {
    public get() {
      return view_as<int>(this) == 555 ? gVal : 0;
    }
    public set(int v) {
      if (view_as<int>(this) == 555)
        gVal = v;
    }
  }
}

public main()
{
  Clam c = Clam(555);
  printnum(c.value);
  printnum(c.value++);
  printnum(c.value--);
  printnum(++c.value);
  printnum(--c.value);
  printnum(c.value);
}
