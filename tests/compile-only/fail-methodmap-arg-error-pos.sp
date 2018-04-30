methodmap Foo {
  public void bar(int[] x) { }
}

public void baz(int[] x) { }

public void main() {
  Foo f;
  f.bar(1);
  baz(1);
}
