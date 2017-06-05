#include <shell>

int c = 3;
int d;
public void main() {
	int a = 3;
	int b;
	b = a = 5;
	printnum(a);
	printnum(b);

	c = d = 5;
	printnum(c);
	printnum(d);
}
