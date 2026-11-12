#include <shell>

int gVal = 0;
int gOther = 0;

methodmap Clam {
  static property int value {
    public get() {
      return gVal;
    }
    public set(int v) {
      gVal = v;
    }
  }

  static property int readOnly {
    public get() {
      return gOther;
    }
  }

  public static int Get() {
    return gVal;
  }
}

public main()
{
  Clam.value = 5;
  printnum(Clam.value);
  printnum(Clam.value++);
  printnum(Clam.value--);
  printnum(++Clam.value);
  printnum(--Clam.value);

  Clam.value += 7;
  printnum(Clam.value);
  printnum(Clam.value += 3);
  printnum(Clam.value);

  Clam.value = Clam.Get() + 1;
  printnum(Clam.value);

  printnum(Clam.readOnly);
  gOther = 42;
  printnum(Clam.readOnly);
}
