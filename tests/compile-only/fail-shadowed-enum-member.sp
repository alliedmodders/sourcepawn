#include <shell>

enum e1 { a, b, c }
enum e2 { b, c, d }

public void main() { 
	new es2[e2];
	es2[b] = 1;
	es2[c] = 2;
	es2[d] = 3;
	printnum(es2[0]);
	printnum(es2[1]);
	printnum(es2[2]);
	printnum(es2[b]);
	printnum(es2[c]);
	printnum(es2[d]);
}
