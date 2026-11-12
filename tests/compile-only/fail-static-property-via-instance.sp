native printnum(t);

int gVal;

methodmap Clam {
  static property int value {
    public get() { return gVal; }
    public set(int v) { gVal = v; }
  }
}

public main() {
  Clam c;
  printnum(c.value);
}
