int gVal;

methodmap Clam {
  static property int value {
    public get() { return gVal; }
  }
}

public main() {
  Clam.value = 5;
}
