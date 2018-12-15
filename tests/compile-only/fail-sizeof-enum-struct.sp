native void printnum(int x);

enum struct X {
  int a;
  int b;
};

public main() {
  X x;
  printnum(sizeof(x[0]));
}
