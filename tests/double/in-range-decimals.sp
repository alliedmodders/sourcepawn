native void printdouble(double n);

public void main() {
	double a = 1.0e-300d;
	double b = 1.0e300d;
	printdouble(a);
	printdouble(b);

	double c = 1.0d;
	double d = 2.0d;
	printdouble(c);
	printdouble(d);
}
