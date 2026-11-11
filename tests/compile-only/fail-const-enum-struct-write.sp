enum struct Inner {
  int x;
}

enum struct Outer {
  Inner i;
  int arr[4];
}

void Mutate(Outer o) {
  o.i.x = 5;
}

void MutateField(int& n) {
  n = 6;
}

void F(const Outer o) {
  o.i.x = 1;
  o.arr[0] = 2;
  Mutate(o);
  MutateField(o.i.x);
}

public void main() {}
