#include <shell>

enum struct A {
	int x;

	bool zero() {
		return this.x == 0;
	}
	bool valid() {
		return this.zero() && this.x == 5;
	}
}

public main() {
	A a;
	a.x = 5;
	printnums(a.x, a.zero(), a.valid());

	if (a.zero())
		print("zero\n");
	if (a.valid())
		print("valid\n");
}
