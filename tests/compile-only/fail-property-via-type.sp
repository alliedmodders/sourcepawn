native printnum(t);

methodmap Clam {
  property int value {
    public get() { return 5; }
  }
}

public main() {
  printnum(Clam.value);
}
