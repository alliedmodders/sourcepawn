#include <shell>

enum struct Child {
  int x;
  int y;
  void setX(int val) { this.x = val;}
}

enum struct Parent {
  Child child;

  void setBlah() {}
}

public void main() {
  Parent p;

  p.setBlah();
  p.child.setX(2); // index tag mismatch (symbol "p") & tag mismatch

  Child c;
  c = p.child; // array must be indexed (variable "c")

  printnum(p.child.x);
  printnum(c.x);
}
