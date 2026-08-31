native void printdouble(double n);

public void main() {
  double a = 10.0d;
  double b = 2.5d;
  double c = 5.0d;

  a += 1.0d;
  printdouble(a);
  a -= b;
  printdouble(a);
  a *= c;
  printdouble(a);
  a /= c;
  printdouble(a);
}
