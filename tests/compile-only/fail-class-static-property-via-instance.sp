int gVal;

class Clam {
  static property int value {
    public get() { return gVal; }
    public set(int v) { gVal = v; }
  }
}

public void main() {
  Clam c = new Clam();
  c.value = 1;
}
