enum struct Foo {
  int x[10];

  int DoStuff() {
    return sizeof(this.x);
  }
}

public main() {
  Foo foo;
  return foo.DoStuff();
}
